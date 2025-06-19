{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson hiding (Options)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as L
import Options.Applicative
import System.FilePath
import System.Exit
import Control.Monad (when)
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)
import qualified Data.Aeson.Types as Aeson
import VideoAssembler.LLM
import Types
import qualified Types.Assembly as Assembly

-- | CLI options
data Options = Options
  { optInputFile      :: FilePath
  , optPromptFile     :: FilePath  
  , optOutputFile     :: FilePath
  , optApiKey         :: Maybe Text
  , optModel          :: Text
  , optTemperature    :: Double
  , optMaxTokens      :: Maybe Int
  , optDebug          :: Bool
  } deriving (Show)

-- | Parse CLI options
options :: Parser Options
options = Options
  <$> strOption
      ( long "input"
     <> short 'i'
     <> metavar "INPUT_FILE"
     <> help "Input JSON file containing media files and metadata" )
  <*> strOption
      ( long "prompt" 
     <> short 'p'
     <> metavar "PROMPT_FILE"
     <> help "Text file containing the user prompt for video editing" )
  <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar "OUTPUT_FILE"
     <> help "Output JSON file for the generated video layout" )
  <*> optional (strOption
      ( long "api-key"
     <> metavar "API_KEY"
     <> help "OpenAI API key (can also be set via OPENAI_API_KEY env var)" ))
  <*> strOption
      ( long "model"
     <> metavar "MODEL"
     <> value "gpt-4o-mini"
     <> help "LLM model to use (default: gpt-4o-mini)" )
  <*> option auto
      ( long "temperature"
     <> metavar "TEMP"
     <> value 0.7
     <> help "LLM temperature (0.0-1.0, default: 0.7)" )
  <*> optional (option auto
      ( long "max-tokens"
     <> metavar "TOKENS"
     <> help "Maximum tokens for LLM response" ))
  <*> switch
      ( long "debug"
     <> short 'd'
     <> help "Enable debug output" )

-- | Program description
opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
 <> progDesc "Generate video layouts using LLM from media files and user prompts"
 <> header "llm-video-editor - AI-powered video editing layout generator" )

-- | Input file format for media files and metadata
data InputData = InputData
  { inputRequestId    :: Text
  , inputMediaFiles   :: [MediaFile]
  , inputSubmittedAt  :: Maybe Text
  , inputConstraints  :: Maybe AssemblyContext
  } deriving (Show)

instance FromJSON InputData where
  parseJSON = withObject "InputData" $ \o -> InputData
    <$> o .: "requestId"
    <*> o .: "mediaFiles"
    <*> o .:? "submittedAt"
    <*> o .:? "constraints"

-- Add missing FromJSON instances
instance FromJSON MediaFile where
  parseJSON = withObject "MediaFile" $ \o -> do
    fileType <- o .: "type"
    case fileType of
      "video" -> Video <$> parseJSON (Object o)
      "photo" -> Photo <$> parseJSON (Object o)
      _ -> fail $ "Unknown media file type: " ++ fileType

instance FromJSON VideoFile where
  parseJSON = withObject "VideoFile" $ \o -> VideoFile
    <$> o .: "metadata"
    <*> (Duration <$> o .: "duration")
    <*> o .: "resolution"
    <*> o .: "frameRate"
    <*> o .: "hasAudio"
    <*> o .: "videoFormat"
    <*> o .:? "contentAnalysis"

instance FromJSON PhotoFile where
  parseJSON = withObject "PhotoFile" $ \o -> PhotoFile
    <$> o .: "metadata"
    <*> o .: "resolution"
    <*> o .: "imageFormat"
    <*> o .:? "cameraSettings"

instance FromJSON MediaMetadata where
  parseJSON = withObject "MediaMetadata" $ \o -> MediaMetadata
    <$> o .: "fileName"
    <*> o .: "filePath"
    <*> o .: "fileSize"
    <*> (Timestamp <$> (o .: "createdAt" >>= parseTimeString))
    <*> o .:? "location"
    <*> o .:? "description"
    <*> o .:? "tags" .!= []
    where
      parseTimeString :: String -> Aeson.Parser UTCTime
      parseTimeString = pure . parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

instance FromJSON VideoContentAnalysis where
  parseJSON = withObject "VideoContentAnalysis" $ \o -> VideoContentAnalysis
    <$> o .: "contentOverview"
    <*> o .: "actionIntroduction"
    <*> o .: "timeBoundDetails"
    <*> o .: "detectedObjects"
    <*> o .: "detectedScenes"
    <*> o .:? "estimatedMood"

instance FromJSON TimeBoundDetail where
  parseJSON = withObject "TimeBoundDetail" $ \o -> TimeBoundDetail
    <$> (Duration <$> o .: "detailStartTime")
    <*> (Duration <$> o .: "detailEndTime")
    <*> o .: "detailDescription"
    <*> o .:? "detailConfidence"

instance FromJSON AssemblyContext where
  parseJSON = withObject "AssemblyContext" $ \o -> AssemblyContext
    <$> o .: "strategy"
    <*> (fmap Duration <$> o .:? "maxVideoDuration")
    <*> o .:? "preferredStyle"
    <*> o .:? "targetAudience"
    <*> o .:? "budgetConstraints"
    <*> o .:? "technicalLimits" .!= []
    <*> o .:? "customInstructions" .!= []

instance FromJSON AssemblyStrategy where
  parseJSON = withObject "AssemblyStrategy" $ \o -> do
    strategyType <- o .: "type"
    case strategyType of
      "SingleLLM" -> SingleLLM <$> o .: "config"
      "SequentialLLM" -> SequentialLLM <$> o .: "configs"
      "HierarchicalAssembly" -> HierarchicalAssembly <$> o .: "masterConfig" <*> o .: "detailConfig"
      "EnsembleAssembly" -> EnsembleAssembly <$> o .: "configs"
      "HybridAssembly" -> HybridAssembly <$> o .: "strategy1" <*> o .: "strategy2"
      _ -> fail $ "Unknown assembly strategy: " ++ strategyType

instance FromJSON LLMConfig where
  parseJSON = withObject "LLMConfig" $ \o -> LLMConfig
    <$> o .: "modelName"
    <*> o .: "temperature"
    <*> o .:? "maxTokens"
    <*> o .:? "systemPrompt"
    <*> o .:? "apiEndpoint"
    <*> o .:? "apiKey"

-- JSON instances are now defined in Types modules

-- | Main CLI entry point
main :: IO ()
main = do
  opts' <- execParser opts
  result <- runCLI opts'
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ err
      exitWith (ExitFailure 1)
    Right _ -> putStrLn "✅ Video layout generated successfully!"

-- | Run the CLI application
runCLI :: Options -> IO (Either String ())
runCLI opts' = do
  when (optDebug opts') $ putStrLn "🔧 Starting LLM Video Editor CLI..."

  -- Read input file
  when (optDebug opts') $ putStrLn $ "📖 Reading input file: " ++ optInputFile opts'
  inputResult <- readInputFile (optInputFile opts')
  case inputResult of
    Left err -> return $ Left $ "Failed to read input file: " ++ err
    Right inputData -> do
      
      -- Read prompt file
      when (optDebug opts') $ putStrLn $ "📝 Reading prompt file: " ++ optPromptFile opts'
      promptResult <- readPromptFile (optPromptFile opts')
      case promptResult of
        Left err -> return $ Left $ "Failed to read prompt file: " ++ err
        Right prompt -> do
          
          -- Create video request
          videoRequest <- createVideoRequest inputData prompt
          
          -- Create LLM config
          let llmConfig = createLLMConfig opts'
          
          -- Create assembly context
          let assemblyContext = maybe defaultAssemblyContext id (inputConstraints inputData)
          
          when (optDebug opts') $ do
            putStrLn $ "🎯 Request ID: " ++ T.unpack (inputRequestId inputData)
            putStrLn $ "📹 Media files: " ++ show (length (inputMediaFiles inputData))
            putStrLn $ "🤖 Model: " ++ T.unpack (modelName llmConfig)
            putStrLn $ "🌡️  Temperature: " ++ show (temperature llmConfig)
          
          -- Generate video layout
          when (optDebug opts') $ putStrLn "🚀 Generating video layout with LLM..."
          result <- createLLMAssembler llmConfig videoRequest assemblyContext
          
          case result of
            Assembly.Success layout -> do
              when (optDebug opts') $ putStrLn "✅ Layout generated successfully"
              
              -- Write output file
              when (optDebug opts') $ putStrLn $ "💾 Writing output file: " ++ optOutputFile opts'
              writeResult <- writeOutputFile (optOutputFile opts') layout
              case writeResult of
                Left err -> return $ Left $ "Failed to write output file: " ++ err
                Right _ -> return $ Right ()
                
            Assembly.Failure err -> return $ Left $ "LLM assembly failed: " ++ show err
            Assembly.PartialSuccess layout warnings -> do
              when (optDebug opts') $ do
                putStrLn "⚠️  Partial success with warnings:"
                mapM_ (putStrLn . ("  - " ++)) (map T.unpack warnings)
              
              -- Write output file anyway
              writeResult <- writeOutputFile (optOutputFile opts') layout
              case writeResult of
                Left err -> return $ Left $ "Failed to write output file: " ++ err
                Right _ -> return $ Right ()

-- | Read and parse input JSON file
readInputFile :: FilePath -> IO (Either String InputData)
readInputFile path = do
  content <- L.readFile path
  case eitherDecode content of
    Left err -> return $ Left err
    Right inputData -> return $ Right inputData

-- | Read prompt text file
readPromptFile :: FilePath -> IO (Either String Text)
readPromptFile path = do
  content <- TIO.readFile path
  return $ Right content

-- | Create video request from input data and prompt
createVideoRequest :: InputData -> Text -> IO VideoRequest
createVideoRequest inputData prompt = do
  -- Use current time if not provided
  timestamp <- case inputSubmittedAt inputData of
    Just _ -> return $ Timestamp $ read "2024-01-15 14:30:00 UTC" -- Placeholder
    Nothing -> do
      -- You would use getCurrentTime here in real implementation
      return $ Timestamp $ read "2024-01-15 14:30:00 UTC"
  
  return $ VideoRequest
    { requestId = inputRequestId inputData
    , mediaFiles = inputMediaFiles inputData
    , userPrompt = prompt
    , submittedAt = timestamp
    }

-- | Create LLM configuration from CLI options
createLLMConfig :: Options -> LLMConfig
createLLMConfig opts' = LLMConfig
  { modelName = optModel opts'
  , temperature = optTemperature opts'
  , maxTokens = optMaxTokens opts'
  , systemPrompt = Nothing
  , apiEndpoint = Nothing
  , apiKey = optApiKey opts'
  }

-- | Default assembly context
defaultAssemblyContext :: AssemblyContext  
defaultAssemblyContext = AssemblyContext
  { strategy = SingleLLM (LLMConfig "gpt-4o-mini" 0.7 (Just 4000) Nothing Nothing Nothing)
  , maxVideoDuration = Nothing
  , preferredStyle = Nothing
  , targetAudience = Nothing
  , budgetConstraints = Nothing
  , technicalLimits = []
  , customInstructions = []
  }

-- | Write video layout to output JSON file
writeOutputFile :: FilePath -> VideoLayout -> IO (Either String ())
writeOutputFile path layout = do
  let content = encodePretty layout
  L.writeFile path content
  return $ Right ()