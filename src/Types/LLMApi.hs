{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.LLMApi
  ( -- * Core Types
    Prompt(..)
  , LLMApi(..)
  , prompt
  -- * API Types
  , LLMRequest(..)
  , LLMMessage(..)
  , LLMResponse(..)
  , LLMChoice(..)
  , RawVideoLayout(..)
  -- * Utility Functions
  , extractJsonFromMarkdown
  , parseResponse
  , addTimestampToLayout
  , formatMediaFiles
  , formatMediaFileReferences
  , formatConstraints
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (parseTimeOrError, defaultTimeLocale)
import GHC.Generics (Generic)
import Types.Assembly (AssemblyResult(..), AssemblyError(..))
import Types.Video (VideoLayout(..), VideoSegment, AudioTrack)
import Types.Common (Duration(..), Resolution, Timestamp(..))
import qualified Types.Assembly as Assembly
import Types.Assembly (AssemblyContext(..))
import Types.Media (VideoRequest(..), MediaFile(..), VideoFile(..), PhotoFile(..), MediaMetadata(..), VideoContentAnalysis(..), fileName)

newtype Prompt = Prompt Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON) 

class LLMApi a where
  call :: a -> Prompt -> IO AssemblyResult

-- | LLMApi API request structure
data LLMRequest = LLMRequest
  { llmMessages :: [LLMMessage]
  , llmModel :: Text
  , llmTemperature :: Double
  , llmMaxTokens :: Maybe Int
  } deriving (Show, Eq, Generic)

data LLMMessage = LLMMessage
  { role :: Text
  , content :: Text
  } deriving (Show, Eq, Generic)

-- | LLMApi API response structure
data LLMResponse = LLMResponse
  { choices :: [LLMChoice]
  } deriving (Show, Eq, Generic)

data LLMChoice = LLMChoice
  { message :: LLMMessage
  } deriving (Show, Eq, Generic)

-- | Raw video layout from LLMApi response (without timestamp)
data RawVideoLayout = RawVideoLayout
  { rawLayoutId :: Text
  , rawTotalDuration :: Duration
  , rawSegments :: [VideoSegment]
  , rawGlobalAudio :: [AudioTrack]
  , rawOutputFormat :: Text
  , rawOutputResolution :: Resolution
  , rawOutputFrameRate :: Double
  } deriving (Show, Eq, Generic)

-- JSON instances
instance ToJSON LLMRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \name -> 
    case name of
      "llmMessages" -> "messages"
      "llmModel" -> "model"
      "llmTemperature" -> "temperature"
      "llmMaxTokens" -> "max_tokens"
      _ -> drop 3 name
  }

instance ToJSON LLMMessage where
  toJSON = genericToJSON defaultOptions

instance FromJSON LLMResponse where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON LLMChoice where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON LLMMessage where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON RawVideoLayout where
  parseJSON = withObject "RawVideoLayout" $ \o -> RawVideoLayout
    <$> o .: "layoutId"
    <*> (Duration <$> o .: "totalDuration")
    <*> o .: "segments"
    <*> o .: "globalAudio"
    <*> o .: "outputFormat"
    <*> o .: "outputResolution"
    <*> o .: "outputFrameRate"

-- | Extract JSON from markdown code blocks
extractJsonFromMarkdown :: Text -> Text
extractJsonFromMarkdown responseText = 
  let cleanText = T.strip responseText
  in if T.isPrefixOf "```json" cleanText
     then 
       let withoutPrefix = T.drop 7 cleanText -- Remove "```json"
           lines' = T.lines withoutPrefix
           jsonLines = takeWhile (not . T.isPrefixOf "```") lines'
       in T.unlines jsonLines
     else if T.isPrefixOf "```" cleanText
     then 
       let withoutPrefix = T.drop 3 cleanText -- Remove "```"
           lines' = T.lines withoutPrefix
           jsonLines = takeWhile (not . T.isPrefixOf "```") lines'
       in T.unlines jsonLines
     else responseText

-- | Parse LLMApi response from text content into AssemblyResult
parseResponse :: Text -> AssemblyResult
parseResponse responseText =
  let cleanedJson = extractJsonFromMarkdown responseText
  in case eitherDecodeStrict' (TE.encodeUtf8 cleanedJson) of
    Left err -> Failure (AssemblyParseError $ T.pack $ "Failed to parse JSON: " ++ err ++ ". Cleaned text: " ++ show cleanedJson)
    Right rawLayout -> 
      -- Add current timestamp to the layout
      case addTimestampToLayout rawLayout of
        Left err -> Failure (AssemblyParseError err)
        Right layout -> Assembly.Success layout

-- | Add current timestamp to parsed layout
addTimestampToLayout :: RawVideoLayout -> Either Text VideoLayout
addTimestampToLayout raw = Right $ VideoLayout
  { layoutId = rawLayoutId raw
  , totalDuration = rawTotalDuration raw
  , segments = rawSegments raw
  , globalAudio = rawGlobalAudio raw
  , outputFormat = rawOutputFormat raw
  , outputResolution = rawOutputResolution raw
  , outputFrameRate = rawOutputFrameRate raw
  , layoutCreatedAt = Timestamp $ parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2024-01-01 00:00:00 UTC"
  }

-- | Typeclass for generating prompts from requests and contexts
prompt :: VideoRequest -> AssemblyContext -> Prompt
prompt request context = Prompt $ T.unlines
  [ "You are a professional video editor AI. Create a detailed video layout from the provided media files and user request."
  , ""
  , "## User Request:"
  , userPrompt request
  , ""
  , "## Available Media Files:"
  , formatMediaFiles (mediaFiles request)
  , ""
  , "## Technical Constraints:"
  , formatConstraints context
  , ""
  , "## CRITICAL: JSON Schema Requirements"
  , "You MUST respond with EXACTLY this JSON structure. Do NOT use markdown formatting, code blocks, or any other text."
  , "Return ONLY valid JSON that matches this schema:"
  , ""
  , "## SegmentType Options:"
  , "- VideoClip: {\"type\": \"VideoClip\", \"mediaId\": \"filename.mp4\", \"startTime\": 0, \"endTime\": 10, \"playbackSpeed\": null}"
  , "- TitleCard: {\"type\": \"TitleCard\", \"text\": \"Title Text\", \"duration\": 3}"
  , "- PhotoClip: {\"type\": \"PhotoClip\", \"mediaId\": \"photo.jpg\", \"duration\": 5}"
  , ""
  , "## Transition Options:"
  , "- Cut: {\"type\": \"Cut\"}"
  , "- Fade: {\"type\": \"Fade\", \"duration\": 1.0}"
  , "- Dissolve: {\"type\": \"Dissolve\", \"duration\": 1.5}"
  , ""
  , "## TextOverlay Format:"
  , "- {\"overlayText\": \"Sample Text\", \"overlayPosition\": [0.5, 0.1], \"overlayDuration\": 3.0, \"overlayStyle\": \"bold\"}"
  , ""
  , "## AudioTrack Format:"
  , "- {\"audioSource\": \"background.mp3\", \"audioStart\": 0, \"audioEnd\": 30, \"volume\": 0.5, \"fadeIn\": 1.0, \"fadeOut\": 1.0}"
  , ""
  , "{"
  , "  \"layoutId\": \"string - unique identifier\","
  , "  \"totalDuration\": \"number - total video duration in seconds\","
  , "  \"segments\": ["
  , "    {"
  , "      \"segmentId\": \"string\","
  , "      \"segmentType\": {"
  , "        \"type\": \"VideoClip\","
  , "        \"mediaId\": \"string - reference to media file name\","
  , "        \"startTime\": \"number - start time in media file\","
  , "        \"endTime\": \"number - end time in media file\","
  , "        \"playbackSpeed\": \"number or null - 1.0 = normal speed\""
  , "      },"
  , "      \"segmentStart\": \"number - start time in seconds\","
  , "      \"segmentEnd\": \"number - end time in seconds\","
  , "      \"textOverlays\": ["
  , "        {"
  , "          \"overlayText\": \"Sample Text\","
  , "          \"overlayPosition\": [0.5, 0.1],"
  , "          \"overlayDuration\": 3.0,"
  , "          \"overlayStyle\": \"bold\""
  , "        }"
  , "      ],"
  , "      \"audioTracks\": ["
  , "        {"
  , "          \"audioSource\": \"segment-audio.mp3\","
  , "          \"audioStart\": 0,"
  , "          \"audioEnd\": 10,"
  , "          \"volume\": 0.8,"
  , "          \"fadeIn\": 0.5,"
  , "          \"fadeOut\": 0.5"
  , "        }"
  , "      ],"
  , "      \"transition\": {\"type\": \"Cut\"}"
  , "    }"
  , "  ],"
  , "  \"globalAudio\": ["
  , "    {"
  , "      \"audioSource\": \"background-music.mp3\","
  , "      \"audioStart\": 0,"
  , "      \"audioEnd\": 30,"
  , "      \"volume\": 0.3,"
  , "      \"fadeIn\": 2.0,"
  , "      \"fadeOut\": 2.0"
  , "    }"
  , "  ],"
  , "  \"outputFormat\": \"mp4\","
  , "  \"outputResolution\": {\"width\": 1920, \"height\": 1080},"
  , "  \"outputFrameRate\": 30.0"
  , "}"
  , ""
  , "## Media File References:"
  , formatMediaFileReferences (mediaFiles request)
  , ""
  , "## IMPORTANT REQUIREMENTS:"
  , "- Include at least one textOverlay with sample text for the main video segment"
  , "- Include at least one audioTrack per segment for sound effects or voice-over"
  , "- Include globalAudio with background music for the entire video"
  , "- Use realistic durations and timing values"
  , "- Ensure all mediaId references match the provided media files"
  , ""
  , "Return ONLY the JSON object. No explanation, no markdown, no code blocks."
  ]

-- | Format media files for prompt
formatMediaFiles :: [MediaFile] -> Text
formatMediaFiles files = T.unlines $ map formatMediaFile files
  where
    formatMediaFile (Video vf) = T.concat
      [ "- Video: ", fileName (videoMetadata vf)
      , " (", T.pack $ show (videoDuration vf), "s)"
      , case contentAnalysis vf of
          Just analysis -> " - " <> contentOverview analysis
          Nothing -> ""
      ]
    formatMediaFile (Photo pf) = T.concat
      [ "- Photo: ", fileName (photoMetadata pf)
      , " (", T.pack $ show (photoResolution pf), ")"
      ]

-- | Format media file references for JSON schema
formatMediaFileReferences :: [MediaFile] -> Text
formatMediaFileReferences files = T.unlines $ map formatMediaReference files
  where
    formatMediaReference (Video vf) = T.concat
      [ "- Use mediaId: \"", fileName (videoMetadata vf), "\" for video clips"
      ]
    formatMediaReference (Photo pf) = T.concat
      [ "- Use mediaId: \"", fileName (photoMetadata pf), "\" for photo clips"
      ]

-- | Format assembly constraints for prompt
formatConstraints :: AssemblyContext -> Text
formatConstraints context = T.unlines $
  [ "- Max duration: " <> maybe "unlimited" (T.pack . show) (maxVideoDuration context)
  , "- Preferred style: " <> maybe "any" id (preferredStyle context)
  , "- Target audience: " <> maybe "general" id (targetAudience context)
  ] ++ map ("- " <>) (technicalLimits context)
    ++ map ("- Custom: " <>) (customInstructions context)