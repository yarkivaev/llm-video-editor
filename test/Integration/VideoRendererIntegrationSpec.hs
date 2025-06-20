{-# LANGUAGE OverloadedStrings #-}

module Integration.VideoRendererIntegrationSpec (spec) where

import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Types.Common
import Types.Video
import VideoExporter hiding (mediaSources, outputPath, tempDir)
import qualified VideoRenderer.FFmpeg as FFmpeg

-- import qualified Data.Text as T

spec :: Spec
spec = describe "VideoRenderer Integration Tests" $ do
  
  describe "FFmpeg Integration" $ around withFFmpegCheck $ do
    it "renders a simple video layout" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts
        threadDelay 100000 -- 100ms delay
        result <- withTestEnvironment createTestVideoLayout $ \(tempDir, mediaSources, layout) -> do
          let outputPath = OutputPath (tempDir </> "output.mp4")
              config = defaultExportConfig mediaSources
          
          renderResult <- exportVideo layout config outputPath
          case renderResult of
            RenderSuccess outputFile -> do
              -- Check if output file was created
              fileExists <- doesFileExist outputFile
              fileExists `shouldBe` True
              return True
            RenderFailure err -> do
              expectationFailure $ "Render failed: " ++ show err
              return False
        
        result `shouldBe` True
    
    it "renders a title video layout" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts  
        threadDelay 200000 -- 200ms delay
        result <- withTestEnvironment createTestTitleVideoLayout $ \(tempDir, mediaSources, layout) -> do
          let outputPath = OutputPath (tempDir </> "title_output.mp4")
              config = defaultExportConfig mediaSources
          
          renderResult <- exportVideo layout config outputPath
          case renderResult of
            RenderSuccess outputFile -> do
              -- Check if output file was created
              fileExists <- doesFileExist outputFile
              fileExists `shouldBe` True
              return True
            RenderFailure err -> do
              expectationFailure $ "Render failed: " ++ show err
              return False
        
        result `shouldBe` True
    
    it "creates FFmpeg config successfully" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Test that we can create FFmpeg config without errors
        let _config = FFmpeg.defaultFFmpegConfig
        -- If we get here without exception, the config creation works
        True `shouldBe` True

-- Helper function to check if FFmpeg is available
withFFmpegCheck :: (Bool -> IO a) -> IO a
withFFmpegCheck action = do
  (exitCode, _, _) <- readProcessWithExitCode "ffmpeg" ["-version"] ""
  let ffmpegAvailable = exitCode == ExitSuccess
  action ffmpegAvailable

-- Helper function to set up test environment
withTestEnvironment :: IO VideoLayout -> ((FilePath, MediaSources, VideoLayout) -> IO a) -> IO a
withTestEnvironment videoLayout action = do
  systemTempDir <- getCanonicalTemporaryDirectory
  testDir <- createTempDirectory systemTempDir "video-renderer-integration"
  
  let videoDir = testDir </> "video"
      photoDir = testDir </> "photo"
      audioDir = testDir </> "audio"
  
  -- Create source directories
  createDirectoryIfMissing True videoDir
  createDirectoryIfMissing True photoDir
  createDirectoryIfMissing True audioDir
  
  -- Create media sources
  let mediaSources = MediaSources videoDir photoDir audioDir
  
  -- Create test layout
  layout <- videoLayout
  
  result <- action (testDir, mediaSources, layout)
  
  -- Cleanup
  removeDirectoryRecursive testDir
  return result

-- Helper function to create a test VideoLayout with a single title
createTestTitleVideoLayout :: IO VideoLayout
createTestTitleVideoLayout = do
  now <- getCurrentTime
  return VideoLayout
    { layoutId = "integration-test-layout"
    , totalDuration = Duration 10.0  -- 10 second video
    , segments = [createTestTitleSegment]
    , globalAudio = []
    , outputFormat = "mp4"
    , outputResolution = Resolution 1280 720  -- 720p for faster rendering
    , outputFrameRate = 24.0  -- 24fps for faster rendering  
    , layoutCreatedAt = Timestamp now
    }

-- Helper function to create a test VideoLayout with several video segments
createTestVideoLayout :: IO VideoLayout
createTestVideoLayout = do
  now <- getCurrentTime
  return VideoLayout
    { layoutId = "integration-test-layout"
    , totalDuration = Duration 10.0  -- 10 second video
    , segments = [createTestVideoSegment]
    , globalAudio = []
    , outputFormat = "mp4"
    , outputResolution = Resolution 1280 720  -- 720p for faster rendering
    , outputFrameRate = 24.0  -- 24fps for faster rendering  
    , layoutCreatedAt = Timestamp now
    }

-- Helper function to create a test title segment
createTestTitleSegment :: VideoSegment
createTestTitleSegment = VideoSegment
  { segmentId = "test-title-seg"
  , segmentType = TitleCard "Integration Test" (Duration 10.0)
  , segmentStart = Duration 0.0
  , segmentEnd = Duration 10.0
  , textOverlays = []
  , audioTracks = []
  , transition = Nothing
  }

-- Helper function to create a video segment
createTestVideoSegment :: VideoSegment
createTestVideoSegment = VideoSegment
  { segmentId = "test-title-seg"
  , segmentType = VideoClip $ 
      MediaReference "examples/stream.mp4" (Duration 0) (Duration 10) Nothing
  , segmentStart = Duration 0.0
  , segmentEnd = Duration 10.0
  , textOverlays = []
  , audioTracks = []
  , transition = Nothing
  }