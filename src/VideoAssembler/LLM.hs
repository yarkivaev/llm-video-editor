{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module VideoAssembler.LLM
  ( LLMVideoAssembler (..)
  , createLLMAssembler
  , generatePrompt
  , parseResponse
  , callLLM
  , formatMediaFiles
  , formatConstraints
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime, UTCTime, parseTimeOrError)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Types
import qualified Types.Assembly as Assembly

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

instance ToJSON LLMRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 3 }

instance ToJSON LLMMessage where
  toJSON = genericToJSON defaultOptions

instance FromJSON LLMResponse where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON LLMChoice where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON LLMMessage where
  parseJSON = genericParseJSON defaultOptions

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

-- | LLM-based video assembler
newtype LLMVideoAssembler = LLMVideoAssembler LLMConfig

-- | Create an LLM video assembler with config
createLLMAssembler :: LLMConfig -> VideoRequest -> AssemblyContext -> IO AssemblyResult
createLLMAssembler config request context = do
  -- Generate prompt from request and context
  let prompt = generatePrompt request context
  
  -- Call LLM API
  llmResponse <- callLLM config prompt
  
  -- Parse response into AssemblyResult
  case llmResponse of
    Left err -> return $ Failure (AssemblyLLMError err)
    Right response -> return $ parseResponse response

-- | Generate LLM prompt from VideoRequest and AssemblyContext
generatePrompt :: VideoRequest -> AssemblyContext -> Text
generatePrompt request context = T.unlines
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
  , "## Instructions:"
  , "Generate a JSON response with the following structure:"
  , "{"
  , "  \"layoutId\": \"unique-id\","
  , "  \"totalDuration\": 120.5,"
  , "  \"segments\": ["
  , "    {"
  , "      \"segmentId\": \"seg-1\","
  , "      \"segmentType\": {"
  , "        \"type\": \"VideoClip\","
  , "        \"mediaId\": \"video-1\","
  , "        \"startTime\": 0.0,"
  , "        \"endTime\": 30.0,"
  , "        \"playbackSpeed\": 1.0"
  , "      },"
  , "      \"segmentStart\": 0.0,"
  , "      \"segmentEnd\": 30.0,"
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
  , "Focus on creating an engaging narrative that matches the user's request."
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

-- | Format assembly constraints for prompt
formatConstraints :: AssemblyContext -> Text
formatConstraints context = T.unlines $
  [ "- Max duration: " <> maybe "unlimited" (T.pack . show) (maxVideoDuration context)
  , "- Preferred style: " <> maybe "any" id (preferredStyle context)
  , "- Target audience: " <> maybe "general" id (targetAudience context)
  ] ++ map ("- " <>) (technicalLimits context)
    ++ map ("- Custom: " <>) (customInstructions context)

-- | Call LLM API
callLLM :: LLMConfig -> Text -> IO (Either Text Text)
callLLM config prompt = do
  manager <- newManager tlsManagerSettings
  
  let request = LLMRequest
        { llmMessages = [LLMMessage "user" prompt]
        , llmModel = modelName config
        , llmTemperature = temperature config
        , llmMaxTokens = maxTokens config
        }
  
  -- For now, return a mock response since we don't have actual API endpoints
  -- In real implementation, this would make HTTP requests to LLM APIs
  return $ Right $ T.unlines
    [ "{"
    , "  \"layoutId\": \"mock-layout-1\","
    , "  \"totalDuration\": 60.0,"
    , "  \"segments\": [],"
    , "  \"globalAudio\": [],"
    , "  \"outputFormat\": \"mp4\","
    , "  \"outputResolution\": {\"width\": 1920, \"height\": 1080},"
    , "  \"outputFrameRate\": 30.0"
    , "}"
    ]

-- | Parse LLM response into AssemblyResult
parseResponse :: Text -> AssemblyResult
parseResponse responseText =
  case eitherDecodeStrict' (TE.encodeUtf8 responseText) of
    Left err -> Failure (AssemblyParseError $ T.pack err)
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

-- JSON instances for RawVideoLayout and related types
instance FromJSON RawVideoLayout where
  parseJSON = withObject "RawVideoLayout" $ \o -> RawVideoLayout
    <$> o .: "layoutId"
    <*> (Duration <$> o .: "totalDuration")
    <*> o .: "segments"
    <*> o .: "globalAudio"
    <*> o .: "outputFormat"
    <*> o .: "outputResolution"
    <*> o .: "outputFrameRate"

instance FromJSON VideoSegment where
  parseJSON = withObject "VideoSegment" $ \o -> VideoSegment
    <$> o .: "segmentId"
    <*> o .: "segmentType"
    <*> (Duration <$> o .: "segmentStart")
    <*> (Duration <$> o .: "segmentEnd")
    <*> o .: "textOverlays"
    <*> o .: "audioTracks"
    <*> o .:? "transition"

instance FromJSON SegmentType where
  parseJSON = withObject "SegmentType" $ \o -> do
    segType <- o .: "type"
    case segType of
      "VideoClip" -> VideoClip <$> parseJSON (Object o)
      "PhotoClip" -> PhotoClip 
        <$> parseJSON (Object o)
        <*> (Duration <$> o .: "duration")
      "TitleCard" -> TitleCard
        <$> o .: "text"
        <*> (Duration <$> o .: "duration")
      _ -> fail $ "Unknown segment type: " ++ segType

instance FromJSON MediaReference where
  parseJSON = withObject "MediaReference" $ \o -> MediaReference
    <$> o .: "mediaId"
    <*> (Duration <$> o .: "startTime")
    <*> (Duration <$> o .: "endTime")
    <*> o .:? "playbackSpeed"

instance FromJSON Resolution where
  parseJSON = withObject "Resolution" $ \o -> Resolution
    <$> o .: "width"
    <*> o .: "height"

instance FromJSON TextOverlay where
  parseJSON = withObject "TextOverlay" $ \o -> TextOverlay
    <$> o .: "overlayText"
    <*> o .: "overlayPosition"
    <*> (Duration <$> o .: "overlayDuration")
    <*> o .:? "overlayStyle"

instance FromJSON AudioTrack where
  parseJSON = withObject "AudioTrack" $ \o -> AudioTrack
    <$> o .: "audioSource"
    <*> (Duration <$> o .: "audioStart")
    <*> (Duration <$> o .: "audioEnd")
    <*> o .: "volume"
    <*> (fmap Duration <$> o .:? "fadeIn")
    <*> (fmap Duration <$> o .:? "fadeOut")

instance FromJSON Transition where
  parseJSON = withObject "Transition" $ \o -> do
    transType <- o .: "type"
    case transType of
      "Cut" -> pure Cut
      "Fade" -> Fade . Duration <$> o .: "duration"
      "Dissolve" -> Dissolve . Duration <$> o .: "duration"
      "Wipe" -> Wipe <$> o .: "wipeType" <*> (Duration <$> o .: "duration")
      "CustomTransition" -> CustomTransition <$> o .: "name" <*> (Duration <$> o .: "duration")
      _ -> fail $ "Unknown transition type: " ++ transType