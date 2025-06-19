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
import VideoExporter
import Types
import qualified Types.Assembly as Assembly

-- | CLI commands
data Command
  = Generate GenerateOptions
  | Render VideoRenderOptions
  deriving (Show)

-- | Generate layout options
data GenerateOptions = GenerateOptions
  { genInputFile      :: FilePath
  , genPromptFile     :: FilePath  
  , genOutputFile     :: FilePath
  , genApiKey         :: Maybe Text
  , genModel          :: Text
  , genTemperature    :: Double
  , genMaxTokens      :: Maybe Int
  , genDebug          :: Bool
  } deriving (Show)

-- | Render video options
data VideoRenderOptions = VideoRenderOptions
  { renderLayoutFile  :: FilePath
  , renderVideoDir    :: FilePath
  , renderPhotoDir    :: FilePath
  , renderAudioDir    :: FilePath
  , renderOutputVideo :: FilePath
  , renderDebug       :: Bool
  } deriving (Show)

-- | Parse CLI commands
commands :: Parser Command
commands = subparser
  ( command "generate" (info generateOptions (progDesc "Generate video layout from media files and prompt"))
  <> command "render" (info videoRenderOptions (progDesc "Render video from layout JSON file"))
  )

-- | Parse generate command options
generateOptions :: Parser Command
generateOptions = Generate <$> (GenerateOptions
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
     <> help "Enable debug output" ))

-- | Parse render command options
videoRenderOptions :: Parser Command
videoRenderOptions = Render <$> (VideoRenderOptions
  <$> strOption
      ( long "layout"
     <> short 'l'
     <> metavar "LAYOUT_FILE"
     <> help "Input JSON file containing video layout" )
  <*> strOption
      ( long "video-dir"
     <> metavar "VIDEO_DIR"
     <> help "Directory containing source video files" )
  <*> strOption
      ( long "photo-dir"
     <> metavar "PHOTO_DIR"
     <> help "Directory containing source photo files" )
  <*> strOption
      ( long "audio-dir"
     <> metavar "AUDIO_DIR"
     <> help "Directory containing source audio files" )
  <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar "OUTPUT_VIDEO"
     <> help "Output video file path" )
  <*> switch
      ( long "debug"
     <> short 'd'
     <> help "Enable debug output" ))

-- | Program description
opts :: ParserInfo Command
opts = info (commands <**> helper)
  ( fullDesc
 <> progDesc "AI-powered video editing: generate layouts from prompts or render videos from layouts"
 <> header "llm-video-editor - AI video editor with layout generation and video rendering" )

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
  cmd <- execParser opts
  result <- runCommand cmd
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ err
      exitWith (ExitFailure 1)
    Right msg -> putStrLn $ "✅ " ++ msg

-- | Run the CLI command
runCommand :: Command -> IO (Either String String)
runCommand (Generate opts') = do
  result <- runGenerate opts'
  case result of
    Left err -> return $ Left err
    Right _ -> return $ Right "Video layout generated successfully!"
runCommand (Render opts') = do
  result <- runRender opts'
  case result of
    Left err -> return $ Left err
    Right _ -> return $ Right "Video rendered successfully!"

-- | Run the generate command
runGenerate :: GenerateOptions -> IO (Either String ())
runGenerate opts' = do
  when (genDebug opts') $ putStrLn "🔧 Starting LLM Video Editor CLI..."

  -- Read input file
  when (genDebug opts') $ putStrLn $ "📖 Reading input file: " ++ genInputFile opts'
  inputResult <- readInputFile (genInputFile opts')
  case inputResult of
    Left err -> return $ Left $ "Failed to read input file: " ++ err
    Right inputData -> do
      
      -- Read prompt file
      when (genDebug opts') $ putStrLn $ "📝 Reading prompt file: " ++ genPromptFile opts'
      promptResult <- readPromptFile (genPromptFile opts')
      case promptResult of
        Left err -> return $ Left $ "Failed to read prompt file: " ++ err
        Right prompt -> do
          
          -- Create video request
          videoRequest <- createVideoRequest inputData prompt
          
          -- Create LLM config
          let llmConfig = createLLMConfig opts'
          
          -- Create assembly context
          let assemblyContext = maybe defaultAssemblyContext id (inputConstraints inputData)
          
          when (genDebug opts') $ do
            putStrLn $ "🎯 Request ID: " ++ T.unpack (inputRequestId inputData)
            putStrLn $ "📹 Media files: " ++ show (length (inputMediaFiles inputData))
            putStrLn $ "🤖 Model: " ++ T.unpack (modelName llmConfig)
            putStrLn $ "🌡️  Temperature: " ++ show (temperature llmConfig)
          
          -- Generate video layout
          when (genDebug opts') $ putStrLn "🚀 Generating video layout with LLM..."
          result <- createLLMAssembler llmConfig videoRequest assemblyContext
          
          case result of
            Assembly.Success layout -> do
              when (genDebug opts') $ putStrLn "✅ Layout generated successfully"
              
              -- Write output file
              when (genDebug opts') $ putStrLn $ "💾 Writing output file: " ++ genOutputFile opts'
              writeResult <- writeOutputFile (genOutputFile opts') layout
              case writeResult of
                Left err -> return $ Left $ "Failed to write output file: " ++ err
                Right _ -> return $ Right ()
                
            Assembly.Failure err -> return $ Left $ "LLM assembly failed: " ++ show err
            Assembly.PartialSuccess layout warnings -> do
              when (genDebug opts') $ do
                putStrLn "⚠️  Partial success with warnings:"
                mapM_ (putStrLn . ("  - " ++)) (map T.unpack warnings)
              
              -- Write output file anyway
              writeResult <- writeOutputFile (genOutputFile opts') layout
              case writeResult of
                Left err -> return $ Left $ "Failed to write output file: " ++ err
                Right _ -> return $ Right ()

-- | Run the render command
runRender :: VideoRenderOptions -> IO (Either String ())
runRender opts' = do
  when (renderDebug opts') $ putStrLn "🔧 Starting Video Renderer..."

  -- Read layout file
  when (renderDebug opts') $ putStrLn $ "📖 Reading layout file: " ++ renderLayoutFile opts'
  
  -- Create media sources
  let mediaSources = createMediaSources (renderVideoDir opts') (renderPhotoDir opts') (renderAudioDir opts')
      config = defaultExportConfig mediaSources
      outputPath = createOutputPath (renderOutputVideo opts')
  
  when (renderDebug opts') $ do
    putStrLn $ "📁 Video source directory: " ++ renderVideoDir opts'
    putStrLn $ "📁 Photo source directory: " ++ renderPhotoDir opts'
    putStrLn $ "📁 Audio source directory: " ++ renderAudioDir opts'
    putStrLn $ "🎬 Output video: " ++ renderOutputVideo opts'
  
  -- Validate configuration
  when (renderDebug opts') $ putStrLn "🔍 Validating export configuration..."
  validationResult <- validateExportConfig config
  case validationResult of
    Left err -> return $ Left $ "Configuration validation failed: " ++ T.unpack err
    Right _ -> do
      
      -- Export video from layout file
      when (renderDebug opts') $ putStrLn "🚀 Rendering video from layout..."
      result <- exportVideoFromFile (renderLayoutFile opts') config outputPath
      
      case result of
        Right (RenderSuccess outputFile) -> do
          when (renderDebug opts') $ putStrLn $ "✅ Video rendered successfully: " ++ outputFile
          return $ Right ()
        Right (RenderFailure err) -> return $ Left $ "Video rendering failed: " ++ show err
        Left parseErr -> return $ Left $ "Failed to parse layout file: " ++ parseErr

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
createLLMConfig :: GenerateOptions -> LLMConfig
createLLMConfig opts' = LLMConfig
  { modelName = genModel opts'
  , temperature = genTemperature opts'
  , maxTokens = genMaxTokens opts'
  , systemPrompt = Nothing
  , apiEndpoint = Nothing
  , apiKey = genApiKey opts'
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