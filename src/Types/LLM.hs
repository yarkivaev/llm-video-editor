{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.LLM
  ( -- * Core Types
    Prompt(..)
  , LLM(..)
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

class LLM a where
  call :: a -> Prompt -> IO AssemblyResult

-- | LLM API request structure
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

-- | LLM API response structure
data LLMResponse = LLMResponse
  { choices :: [LLMChoice]
  } deriving (Show, Eq, Generic)

data LLMChoice = LLMChoice
  { message :: LLMMessage
  } deriving (Show, Eq, Generic)

-- | Raw video layout from LLM response (without timestamp)
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

-- | Parse LLM response from text content into AssemblyResult
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
  , "{"
  , "  \"layoutId\": \"string - unique identifier\","
  , "  \"totalDuration\": \"number - total video duration in seconds\","
  , "  \"segments\": ["
  , "    {"
  , "      \"segmentId\": \"string\","
  , "      \"segmentType\": {"
  , "        \"type\": \"TitleCard|VideoClip|PhotoClip|Transition\","
  , "        \"text\": \"string (for TitleCard)\","
  , "        \"duration\": \"number\""
  , "      },"
  , "      \"segmentStart\": \"number - start time in seconds\","
  , "      \"segmentEnd\": \"number - end time in seconds\","
  , "      \"textOverlays\": [],"
  , "      \"audioTracks\": [],"
  , "      \"transition\": null"
  , "    }"
  , "  ],"
  , "  \"globalAudio\": [],"
  , "  \"outputFormat\": \"mp4\","
  , "  \"outputResolution\": {\"width\": 1920, \"height\": 1080},"
  , "  \"outputFrameRate\": 30.0"
  , "}"
  , ""
  , "## Media File References:"
  , formatMediaFileReferences (mediaFiles request)
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