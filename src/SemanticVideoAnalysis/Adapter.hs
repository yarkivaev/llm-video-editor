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
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Process (readProcess)
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
    
    result <- tryReadProcess cmd args ""
    case result of
        Left ex -> return $ Left $ "CLI execution failed: " ++ show ex
        Right output -> parseAnalysisOutput output
  where
    tryReadProcess c a i = (Right <$> readProcess c a i) `catch` (\ex -> return $ Left (ex :: SomeException))

-- | Build command line arguments for analysis
buildAnalysisArgs :: AnalysisOptions -> Text -> [String]
buildAnalysisArgs options filePath = 
    [ "analyze"
    , T.unpack filePath
    , "--frames", show $ frames options
    , "--device", deviceToString $ device options
    , "--model", T.unpack $ model options
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
    -- For now, create a simple mock analysis
    -- In real implementation, this would parse the JSON output from the CLI
    return $ Right $ VideoContentAnalysis
        { contentOverview = "Video content analyzed by semantic-video-analysis"
        , actionIntroduction = "Analysis generated from extracted frames"
        , timeBoundDetails = 
            [ TimeBoundDetail 
                { detailStartTime = Duration 0
                , detailEndTime = Duration 10
                , detailDescription = "Frame-based content analysis"
                , detailConfidence = Just 0.8
                }
            ]
        , detectedObjects = ["object1", "object2"]
        , detectedScenes = ["indoor", "outdoor"]
        , estimatedMood = Just "neutral"
        }