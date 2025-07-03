{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SemanticVideoAnalysis.SemanticVideoAnalysisIntegrationSpec (spec) where

import Test.Hspec
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Process (readProcess)
import System.Exit (ExitCode(..))
import System.Directory (removeFile, removeDirectoryRecursive, doesFileExist, doesDirectoryExist)
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T

import SemanticVideoAnalysis.Adapter
import Types.VideoAnalysis (VideoAnalysis(..))
import Types.Media (VideoContentAnalysis(..))
import File (File(..), Path(..), Segment(..))
import FileSystem (MonadFileShow(..))
import Absolute.Common (resolvePath)

-- Mock MonadFileShow instance for testing
newtype TestAdapter a = TestAdapter { runTestAdapter :: ReaderT SemanticVideoAnalysisConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader SemanticVideoAnalysisConfig, MonadIO)

instance MonadFileShow TestAdapter where
  showPath _ = return "test/path"
  showFile file = return $ T.pack $ resolvePath file

-- Test configuration
testConfig :: SemanticVideoAnalysisConfig
testConfig = SemanticVideoAnalysisConfig
  { binaryPath = "integration-test/resources/semantic-video-analysis/venv/bin/semantic-video-analysis"
  , defaultFrames = 3
  , defaultDevice = CPU
  , defaultModel = "Salesforce/blip-image-captioning-base"
  }

-- Test file
testVideoFile :: File
testVideoFile = File 
  { filePath = Path [Segment "integration-test", Segment "resources", Segment "videos"]
  , fileName = "test-video-1.mp4"
  }

-- Cleanup function to remove generated files
cleanupGeneratedFiles :: IO ()
cleanupGeneratedFiles = do
  -- Remove extracted frames directory
  framesExist <- doesDirectoryExist "extracted_frames"
  when framesExist $ do
    result <- try $ removeDirectoryRecursive "extracted_frames"
    case result of
      Left ex -> putStrLn $ "Warning: Could not remove extracted_frames: " ++ show (ex :: SomeException)
      Right _ -> return ()
  
  -- Remove description JSON file
  descriptionExists <- doesFileExist "test-video-1_description.json"
  when descriptionExists $ do
    result <- try $ removeFile "test-video-1_description.json"
    case result of
      Left ex -> putStrLn $ "Warning: Could not remove test-video-1_description.json: " ++ show (ex :: SomeException)
      Right _ -> return ()

spec :: Spec
spec = describe "SemanticVideoAnalysis Integration Tests" $ do
  
  describe "CLI Integration" $ do
    it "should have semantic-video-analysis available" $ do
      result <- readProcess "integration-test/resources/semantic-video-analysis/venv/bin/semantic-video-analysis" ["--help"] ""
      result `shouldContain` "Extract semantic descriptions from video files"
    
    it "should be able to run init command" $ do
      result <- readProcess "integration-test/resources/semantic-video-analysis/venv/bin/semantic-video-analysis" ["init"] ""
      -- Should not throw an exception
      result `shouldSatisfy` (not . null)
  
  describe "Adapter Integration" $ do
    it "should initialize semantic video analysis" $ do
      result <- runReaderT initializeSemanticVideoAnalysis testConfig
      case result of
        Left err -> expectationFailure $ "Initialization failed: " ++ err
        Right _ -> return ()
    
    it "should analyze video using VideoAnalysis instance" $ do
      result <- runReaderT (runTestAdapter $ analyze testVideoFile) testConfig
      
      -- Basic validation of the analysis result
      contentOverview result `shouldSatisfy` (not . T.null)
      actionIntroduction result `shouldSatisfy` (not . T.null)
      detectedObjects result `shouldSatisfy` (not . null)
      detectedScenes result `shouldSatisfy` (not . null)
      
      -- Cleanup generated files
      cleanupGeneratedFiles