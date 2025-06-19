{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module VideoAssembler.LLM
  ( LLMVideoAssembler (..)
  , createLLMAssembler
  , generatePrompt
  , parseResponse
  , callLLM
  , formatMediaFiles
  , formatMediaFileReferences
  , formatConstraints
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as L
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime, UTCTime, parseTimeOrError)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Types
import qualified Types.Assembly as Assembly

-- | Debug logging for integration tests
debugLog :: String -> IO ()
debugLog msg = do
  debugMode <- lookupEnv "DEBUG_LLM_API"
  case debugMode of
    Just "true" -> hPutStrLn stderr $ "[DEBUG LLM] " ++ msg
    _ -> return ()

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
  , "## CRITICAL: JSON Schema Requirements"
  , "You MUST respond with EXACTLY this JSON structure. Do NOT use markdown formatting, code blocks, or any other text."
  , "Return ONLY valid JSON that matches this schema:"
  , ""
  , "{"
  , "  \"layoutId\": \"string - unique identifier\","
  , "  \"totalDuration\": \"number - total video duration in seconds\","
  , "  \"segments\": ["
  , "    {"
  , "      \"segmentId\": \"string - unique segment identifier\","
  , "      \"segmentType\": {"
  , "        \"type\": \"VideoClip | PhotoClip | TitleCard\","
  , "        \"mediaId\": \"string - reference to media file (required for VideoClip/PhotoClip)\","
  , "        \"startTime\": \"number - start time in source media (required for VideoClip)\","
  , "        \"endTime\": \"number - end time in source media (required for VideoClip)\","
  , "        \"playbackSpeed\": \"number - playback speed multiplier (optional for VideoClip, default 1.0)\","
  , "        \"text\": \"string - title text (required for TitleCard)\","
  , "        \"duration\": \"number - display duration in seconds (required for PhotoClip and TitleCard)\""
  , "      },"
  , "      \"segmentStart\": \"number - start time in final video\","
  , "      \"segmentEnd\": \"number - end time in final video\","
  , "      \"textOverlays\": ["
  , "        {"
  , "          \"overlayText\": \"string - text to display\","
  , "          \"overlayPosition\": [\"number\", \"number\"] // [x, y] coordinates from 0.0 to 1.0,"
  , "          \"overlayDuration\": \"number - duration in seconds\","
  , "          \"overlayStyle\": \"string - optional style name\""
  , "        }"
  , "      ],"
  , "      \"audioTracks\": ["
  , "        {"
  , "          \"audioSource\": \"string - audio file name or identifier\","
  , "          \"audioStart\": \"number - start time in seconds\","
  , "          \"audioEnd\": \"number - end time in seconds\","
  , "          \"volume\": \"number - volume level 0.0 to 1.0\","
  , "          \"fadeIn\": \"number - fade in duration in seconds (optional)\","
  , "          \"fadeOut\": \"number - fade out duration in seconds (optional)\""
  , "        }"
  , "      ],"
  , "      \"transition\": {"
  , "        \"type\": \"Cut | Fade | Dissolve | Wipe | CustomTransition\","
  , "        \"duration\": \"number - transition duration in seconds (required for non-Cut)\","
  , "        \"wipeType\": \"string - wipe direction (required for Wipe)\","
  , "        \"name\": \"string - custom transition name (required for CustomTransition)\""
  , "      }"
  , "    }"
  , "  ],"
  , "  \"globalAudio\": ["
  , "    {"
  , "      \"audioSource\": \"string - background audio file\","
  , "      \"audioStart\": \"number - start time\","
  , "      \"audioEnd\": \"number - end time\","
  , "      \"volume\": \"number - volume level\","
  , "      \"fadeIn\": \"number - optional fade in\","
  , "      \"fadeOut\": \"number - optional fade out\""
  , "    }"
  , "  ],"
  , "  \"outputFormat\": \"string - output format (mp4, mov, etc)\","
  , "  \"outputResolution\": {"
  , "    \"width\": \"number - output width in pixels\","
  , "    \"height\": \"number - output height in pixels\""
  , "  },"
  , "  \"outputFrameRate\": \"number - output frame rate (fps)\""
  , "}"
  , ""
  , "## FIELD REQUIREMENTS:"
  , "- Use ONLY the exact field names shown above"
  , "- For VideoClip: MUST include mediaId, startTime, endTime"
  , "- For PhotoClip: MUST include mediaId, duration"
  , "- For TitleCard: MUST include text, duration"
  , "- overlayPosition MUST be array of exactly 2 numbers [x, y]"
  , "- transition can be null for Cut transitions"
  , "- Arrays can be empty [] if no overlays/audio tracks needed"
  , ""
  , "## MEDIA FILE REFERENCES:"
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
      , " (total duration: ", T.pack $ show (videoDuration vf), "s)"
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

-- | Call LLM API (real ChatGPT implementation)
callLLM :: LLMConfig -> Text -> IO (Either Text Text)
callLLM config prompt = do
  -- Check if we should use real API or mock
  useRealAPI <- lookupEnv "USE_REAL_CHATGPT_API"
  case useRealAPI of
    Just "true" -> callRealChatGPT config prompt
    _ -> callMockLLM config prompt

-- | Call real ChatGPT API
callRealChatGPT :: LLMConfig -> Text -> IO (Either Text Text)
callRealChatGPT config prompt = do
  debugLog "Starting ChatGPT API call"
  
  maybeApiKey <- case apiKey config of
    Just key -> return $ Just key
    Nothing -> fmap T.pack <$> lookupEnv "OPENAI_API_KEY"
  
  case maybeApiKey of
    Nothing -> return $ Left "No OpenAI API key provided. Set OPENAI_API_KEY environment variable or provide it in LLMConfig."
    Just apiKeyValue -> do
      manager <- newManager tlsManagerSettings
      
      let llmRequest = LLMRequest
            { llmMessages = [LLMMessage "user" prompt]
            , llmModel = modelName config
            , llmTemperature = temperature config
            , llmMaxTokens = maxTokens config
            }
      
      let requestBody = encode llmRequest
      let url = case apiEndpoint config of
            Just endpoint -> T.unpack endpoint
            Nothing -> "https://api.openai.com/v1/chat/completions"
      
      -- Debug log the request
      debugLog $ "API URL: " ++ url
      debugLog $ "Model: " ++ T.unpack (modelName config)
      debugLog $ "Temperature: " ++ show (temperature config)
      debugLog $ "Max tokens: " ++ show (maxTokens config)
      debugLog $ "Prompt length: " ++ show (T.length prompt)
      debugLog $ "Request body JSON: " ++ show (L.take 1000 requestBody) -- First 1000 chars
      
      initialRequest <- parseRequest url
      let request = initialRequest
            { method = "POST"
            , requestHeaders = 
                [ ("Content-Type", "application/json")
                , ("Authorization", "Bearer " <> TE.encodeUtf8 apiKeyValue)
                ]
            , requestBody = RequestBodyLBS requestBody
            }
      
      debugLog "Sending HTTP request to OpenAI..."
      response <- httpLbs request manager
      let statusCode' = statusCode $ responseStatus response
      let responseBodyData = responseBody response
      
      debugLog $ "Received response with status code: " ++ show statusCode'
      debugLog $ "Response body size: " ++ show (L.length responseBodyData) ++ " bytes"
      debugLog $ "Response body (first 1000 chars): " ++ show responseBodyData
      
      if statusCode' == 200
        then case eitherDecode responseBodyData of
          Left err -> do
            debugLog $ "Failed to parse JSON response: " ++ err
            return $ Left $ T.pack $ "Failed to parse ChatGPT response: " ++ err
          Right llmResponse -> case extractContent llmResponse of
            Nothing -> do
              debugLog "No content found in parsed response"
              return $ Left "No content found in ChatGPT response"
            Just content -> do
              debugLog $ "Successfully extracted content, length: " ++ show (T.length content)
              return $ Right content
        else do
          debugLog $ "API error - Status: " ++ show statusCode' ++ ", Body: " ++ show responseBodyData
          return $ Left $ T.pack $ "ChatGPT API error. Status code: " ++ show statusCode' ++ ". Response: " ++ show responseBodyData

-- | Extract content from ChatGPT response
extractContent :: LLMResponse -> Maybe Text
extractContent response = do
  choice <- case choices response of
    [] -> Nothing
    (c:_) -> Just c
  return $ content $ message choice

-- | Mock LLM call for testing without API
callMockLLM :: LLMConfig -> Text -> IO (Either Text Text)
callMockLLM config prompt = do
  return $ Right $ T.unlines
    [ "{"
    , "  \"layoutId\": \"mock-layout-1\","
    , "  \"totalDuration\": 60.0,"
    , "  \"segments\": ["
    , "    {"
    , "      \"segmentId\": \"seg-1\","
    , "      \"segmentType\": {"
    , "        \"type\": \"TitleCard\","
    , "        \"text\": \"My Travel Video\","
    , "        \"duration\": 3.0"
    , "      },"
    , "      \"segmentStart\": 0.0,"
    , "      \"segmentEnd\": 3.0,"
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
    ]

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

-- | Parse LLM response into AssemblyResult
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
        <$> (do -- Create MediaReference for photo clip
              mediaId <- o .: "mediaId"
              return $ MediaReference mediaId (Duration 0.0) (Duration 0.0) Nothing)
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
  parseJSON value = case value of
    -- Handle null input for Cut transitions
    Null -> pure Cut
    -- Handle proper object input
    Object o -> do
      transType <- o .: "type"
      case transType of
        "Cut" -> pure Cut
        "Fade" -> Fade . Duration <$> o .: "duration"
        "Dissolve" -> Dissolve . Duration <$> o .: "duration"
        "Wipe" -> Wipe <$> o .: "wipeType" <*> (Duration <$> o .: "duration")
        "CustomTransition" -> CustomTransition <$> o .: "name" <*> (Duration <$> o .: "duration")
        _ -> fail $ "Unknown transition type: " ++ transType
    _ -> fail "Expected Object or null for Transition"

instance FromJSON VideoLayout where
  parseJSON = withObject "VideoLayout" $ \o -> do
    layoutId <- o .: "layoutId"
    totalDuration <- Duration <$> o .: "totalDuration"
    segments <- o .: "segments"
    globalAudio <- o .: "globalAudio"
    outputFormat <- o .: "outputFormat"
    outputResolution <- o .: "outputResolution"
    outputFrameRate <- o .: "outputFrameRate"
    layoutCreatedAt <- Timestamp <$> o .: "layoutCreatedAt"
    return VideoLayout{..}