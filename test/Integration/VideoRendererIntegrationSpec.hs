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
import VideoExporter
import VideoRenderer.FFmpeg

spec :: Spec
spec = describe "VideoRenderer Integration Tests" $ do
  
  describe "FFmpeg Integration" $ around withFFmpegCheck $ do
    it "renders a simple video layout" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts
        threadDelay 100000 -- 100ms delay
        result <- withTestEnvironment $ \(tempDir, mediaSources, layout) -> do
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
    
    it "validates render context properly" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts  
        threadDelay 200000 -- 200ms delay
        result <- withTestEnvironment $ \(tempDir, mediaSources, layout) -> do
          let outputPath = OutputPath (tempDir </> "output.mp4")
              context = RenderContext mediaSources outputPath defaultRenderOptions
              config = defaultFFmpegConfig
          
          validationResult <- runFFmpegRenderer config $ validateRender layout context
          case validationResult of
            Right _ -> return True
            Left err -> do
              expectationFailure $ "Validation failed: " ++ show err
              return False
        
        result `shouldBe` True
    
    it "estimates render time" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts
        threadDelay 300000 -- 300ms delay
        result <- withTestEnvironment $ \(_, mediaSources, layout) -> do
          let context = RenderContext mediaSources (OutputPath "/tmp/test.mp4") defaultRenderOptions
              config = defaultFFmpegConfig
          
          estimatedTime <- runFFmpegRenderer config $ estimateRenderTime layout context
          -- Should be positive and reasonable (between 10 and 300 seconds for test video)
          return $ estimatedTime > 0 && estimatedTime < 300
        
        result `shouldBe` True
    
    it "returns correct capabilities" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts
        threadDelay 400000 -- 400ms delay
        let config = defaultFFmpegConfig
        capabilities <- runFFmpegRenderer config getRendererCapabilities
        capabilities `shouldContain` ["ffmpeg-based"]
        capabilities `shouldContain` ["supports_mp4"]

-- Helper function to check if FFmpeg is available
withFFmpegCheck :: (Bool -> IO a) -> IO a
withFFmpegCheck action = do
  (exitCode, _, _) <- readProcessWithExitCode "ffmpeg" ["-version"] ""
  let ffmpegAvailable = exitCode == ExitSuccess
  action ffmpegAvailable

-- Helper function to set up test environment
withTestEnvironment :: ((FilePath, MediaSources, VideoLayout) -> IO a) -> IO a
withTestEnvironment action = do
  tempDir <- getCanonicalTemporaryDirectory
  testDir <- createTempDirectory tempDir "video-renderer-integration"
  
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
  layout <- createTestVideoLayout
  
  result <- action (testDir, mediaSources, layout)
  
  -- Cleanup
  removeDirectoryRecursive testDir
  return result

-- Helper function to create a test VideoLayout
createTestVideoLayout :: IO VideoLayout
createTestVideoLayout = do
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

-- Helper function to create a test segment
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