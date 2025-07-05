{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticVideoAnalysis.Adapter 
    ( SemanticVideoAnalysisAdapter
    , SemanticVideoAnalysisConfig (..)
    , Device (..)
    , AnalysisOptions (..)
    , initializeSemanticVideoAnalysis
    ) where

import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.Process (readProcess, readProcessWithExitCode)
import qualified System.Directory
import Types.Media (VideoContentAnalysis(..), TimeBoundDetail(..))
import Types.VideoAnalysis (VideoAnalysis(..))
import Types.Common (Duration(..))
import File
import FileSystem (MonadFileShow(..))

-- | Supported devices for analysis
data Device = CPU | CUDA
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Configuration for semantic video analysis
data SemanticVideoAnalysisConfig = SemanticVideoAnalysisConfig
  { binaryPath :: Text
  , defaultFrames :: Int
  , defaultDevice :: Device
  , defaultModel :: Text
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Options for video analysis
data AnalysisOptions = AnalysisOptions
  { frames :: Int
  , device :: Device
  , outputDir :: Maybe Text
  , model :: Text
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Reader monad for semantic video analysis
type SemanticVideoAnalysisAdapter = ReaderT SemanticVideoAnalysisConfig IO

-- | Initialize semantic video analysis (downloads models)
initializeSemanticVideoAnalysis :: (MonadIO m, MonadReader SemanticVideoAnalysisConfig m) => m (Either String ())
initializeSemanticVideoAnalysis = do
    config <- ask
    let cmd = T.unpack $ binaryPath config
        args = ["init"]
    
    result <- liftIO $ tryReadProcess cmd args ""
    case result of
        Left ex -> return $ Left $ "Initialization failed: " ++ show ex
        Right _ -> return $ Right ()
  where
    tryReadProcess c a i = (Right <$> readProcess c a i) `catch` (\ex -> return $ Left (ex :: SomeException))

-- | VideoAnalysis instance for SemanticVideoAnalysisAdapter
instance (Monad m, MonadFileShow m, MonadReader SemanticVideoAnalysisConfig m, MonadIO m) => VideoAnalysis m where
    analyze file = do
        config <- ask
        let options = AnalysisOptions
              { frames = defaultFrames config
              , device = defaultDevice config
              , outputDir = Nothing
              , model = defaultModel config
              }
        analyzeWithOptions options file

-- | Analyze video with custom options
analyzeWithOptions :: (MonadFileShow m, MonadReader SemanticVideoAnalysisConfig m, MonadIO m) => AnalysisOptions -> File -> m VideoContentAnalysis
analyzeWithOptions options file = do
    config <- ask
    filePath <- showFile file
    result <- liftIO $ runSemanticVideoAnalysisCLI config options filePath
    case result of
        Left err -> error $ "Semantic video analysis failed: " ++ err
        Right analysis -> return analysis

-- | Run the semantic-video-analysis CLI tool
runSemanticVideoAnalysisCLI :: SemanticVideoAnalysisConfig -> AnalysisOptions -> Text -> IO (Either String VideoContentAnalysis)
runSemanticVideoAnalysisCLI config options filePath = do
    let cmd = T.unpack $ binaryPath config
        args = buildAnalysisArgs options filePath
    
    -- First check if the file exists
    fileExists <- liftIO $ System.Directory.doesFileExist (T.unpack filePath)
    if not fileExists
        then return $ Left $ "Video file not found: " ++ T.unpack filePath
        else do
            result <- tryReadProcessWithStderr cmd args ""
            case result of
                Left (exitCode, stdout, stderr) -> 
                    return $ Left $ unlines 
                        [ "CLI execution failed with exit code: " ++ show exitCode
                        , "Command: " ++ cmd ++ " " ++ unwords args
                        , "Stdout: " ++ stdout
                        , "Stderr: " ++ stderr
                        ]
                Right output -> parseAnalysisOutput output
  where
    tryReadProcessWithStderr c a i = do
        (exitCode, stdout, stderr) <- readProcessWithExitCode c a i
        case exitCode of
            ExitSuccess -> return $ Right stdout
            ExitFailure code -> return $ Left (code, stdout, stderr)

-- | Build command line arguments for analysis
buildAnalysisArgs :: AnalysisOptions -> Text -> [String]
buildAnalysisArgs options filePath = 
    [ "analyze"
    , T.unpack filePath
    , "--frames", show $ frames options
    , "--device", deviceToString $ device options
    , "--model", T.unpack $ model options
    , "--json"  -- Always request JSON output for machine processing
    ] ++ outputDirArgs
  where
    deviceToString CPU = "cpu"
    deviceToString CUDA = "cuda"
    
    outputDirArgs = case outputDir options of
        Nothing -> []
        Just dir -> ["--output-dir", T.unpack dir]

-- | Parse the output from semantic-video-analysis CLI
parseAnalysisOutput :: String -> IO (Either String VideoContentAnalysis)
parseAnalysisOutput output = do
    -- First, try to parse as JSON from the semantic-video-analysis output
    case eitherDecode (L8.pack output) of
        Left jsonErr -> 
            -- If JSON parsing fails, check if output contains actual content
            if null output || all (`elem` [' ', '\n', '\t']) output
                then return $ Left "Empty output from semantic-video-analysis"
                else return $ Left $ "Failed to parse JSON output: " ++ jsonErr ++ "\nOutput was: " ++ take 500 output
        Right analysisData ->
            -- Convert from the CLI output format to our internal format
            return $ Right $ convertAnalysisData analysisData
  where
    -- Convert from semantic-video-analysis JSON format to VideoContentAnalysis
    convertAnalysisData :: AnalysisOutput -> VideoContentAnalysis
    convertAnalysisData ao = VideoContentAnalysis
        { contentOverview = overview ao
        , actionIntroduction = action_introduction ao
        , timeBoundDetails = map convertTimeBound (time_bound_details ao)
        , detectedObjects = detected_objects ao
        , detectedScenes = detected_scenes ao
        , estimatedMood = estimated_mood ao
        }
    
    convertTimeBound :: TimeBoundOutput -> TimeBoundDetail
    convertTimeBound tbo = TimeBoundDetail
        { detailStartTime = Duration (start_time tbo)
        , detailEndTime = Duration (end_time tbo)
        , detailDescription = description tbo
        , detailConfidence = confidence tbo
        }

-- | Output format from semantic-video-analysis CLI
data AnalysisOutput = AnalysisOutput
    { overview :: Text
    , action_introduction :: Text
    , time_bound_details :: [TimeBoundOutput]
    , detected_objects :: [Text]
    , detected_scenes :: [Text]
    , estimated_mood :: Maybe Text
    } deriving (Generic, FromJSON)

data TimeBoundOutput = TimeBoundOutput
    { start_time :: Double
    , end_time :: Double
    , description :: Text
    , confidence :: Maybe Double
    } deriving (Generic, FromJSON)