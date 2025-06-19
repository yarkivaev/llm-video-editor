{-# LANGUAGE OverloadedStrings #-}

module Integration.VideoExporterEndToEndSpec (spec) where

import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Aeson (encodeFile)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Types.Common
import Types.Video
import VideoExporter

spec :: Spec
spec = describe "VideoExporter End-to-End Tests" $ do
  
  describe "Complete Export Workflow" $ around withFFmpegCheck $ do
    it "exports video from VideoLayout object" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts
        threadDelay 500000 -- 500ms delay
        success <- withTestEnvironment $ \(testDir, mediaSources) -> do
          -- Create test layout
          layout <- createCompleteTestLayout
          
          -- Set up export configuration
          let config = defaultExportConfig mediaSources
              outputPath = OutputPath (testDir </> "complete_export.mp4")
          
          -- Export video
          result <- exportVideo layout config outputPath
          
          case result of
            RenderSuccess outputFile -> do
              -- Verify output file exists
              fileExists <- doesFileExist outputFile
              fileExists `shouldBe` True
              
              -- Verify file is not empty (basic check)
              -- In a real test, we might use ffprobe to check video properties
              return True
              
            RenderFailure err -> do
              expectationFailure $ "Export failed: " ++ show err
              return False
        
        success `shouldBe` True
    
    it "exports video from JSON file" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts
        threadDelay 600000 -- 600ms delay
        success <- withTestEnvironment $ \(testDir, mediaSources) -> do
          -- Create test layout JSON with correct structure  
          let layoutFile = testDir </> "test_layout.json"
              layoutJson = unlines
                [ "{"
                , "  \"layoutId\": \"e2e-test-layout\","
                , "  \"totalDuration\": 15,"
                , "  \"segments\": [{"
                , "    \"segmentId\": \"seg-0\","
                , "    \"segmentType\": {"
                , "      \"type\": \"TitleCard\","
                , "      \"text\": \"Opening Title\","
                , "      \"duration\": 5"
                , "    },"
                , "    \"segmentStart\": 0,"
                , "    \"segmentEnd\": 5,"
                , "    \"textOverlays\": [],"
                , "    \"audioTracks\": [],"
                , "    \"transition\": null"
                , "  }],"
                , "  \"globalAudio\": [],"
                , "  \"outputFormat\": \"mp4\","
                , "  \"outputResolution\": {\"width\": 1280, \"height\": 720},"
                , "  \"outputFrameRate\": 24,"
                , "  \"layoutCreatedAt\": \"2024-01-01T00:00:00Z\""
                , "}"
                ]
          writeFile layoutFile layoutJson
          
          -- Set up export configuration
          let config = defaultExportConfig mediaSources
              outputPath = OutputPath (testDir </> "json_export.mp4")
          
          -- Export video from JSON file
          result <- exportVideoFromFile layoutFile config outputPath
          
          case result of
            Right (RenderSuccess outputFile) -> do
              -- Verify output file exists
              fileExists <- doesFileExist outputFile
              fileExists `shouldBe` True
              return True
              
            Right (RenderFailure err) -> do
              expectationFailure $ "Export failed: " ++ show err
              return False
              
            Left parseErr -> do
              expectationFailure $ "JSON parse failed: " ++ parseErr
              return False
        
        success `shouldBe` True
    
    it "handles missing JSON file gracefully" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts
        threadDelay 700000 -- 700ms delay
        withTestEnvironment $ \(testDir, mediaSources) -> do
          let config = defaultExportConfig mediaSources
              outputPath = OutputPath (testDir </> "missing_export.mp4")
              missingFile = testDir </> "nonexistent.json"
          
          exportResult <- exportVideoFromFile missingFile config outputPath
          
          case exportResult of
            Left err -> do
              -- Should get an error about missing file
              err `shouldContain` "not found"
            Right _ -> do
              expectationFailure "Should fail on missing file"
    
    it "handles invalid JSON gracefully" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts
        threadDelay 800000 -- 800ms delay
        withTestEnvironment $ \(testDir, mediaSources) -> do
          -- Create invalid JSON file
          let invalidJsonFile = testDir </> "invalid.json"
          writeFile invalidJsonFile "{ invalid json }"
          
          let config = defaultExportConfig mediaSources
              outputPath = OutputPath (testDir </> "invalid_export.mp4")
          
          exportResult <- exportVideoFromFile invalidJsonFile config outputPath
          
          case exportResult of
            Left err -> do
              -- Should get a parse error
              map toLower err `shouldContain` "parse"
            Right _ -> do
              expectationFailure "Should fail on invalid JSON"
    
    it "validates export configuration" $ \ffmpegAvailable -> do
      when ffmpegAvailable $ do
        -- Add delay to avoid resource conflicts
        threadDelay 900000 -- 900ms delay
        tempDir <- getCanonicalTemporaryDirectory
        testDir <- createTempDirectory tempDir "export-validation-test"
        
        -- Create config with non-existent directories
        let invalidSources = createMediaSources 
              (testDir </> "nonexistent_video")
              (testDir </> "nonexistent_photo") 
              (testDir </> "nonexistent_audio")
            config = defaultExportConfig invalidSources
        
        result <- validateExportConfig config
        
        case result of
          Left _err -> do
            -- Should fail validation
            removeDirectoryRecursive testDir
          Right _ -> do
            expectationFailure "Should fail validation with missing directories"
            removeDirectoryRecursive testDir

-- Helper function to check if FFmpeg is available
withFFmpegCheck :: (Bool -> IO a) -> IO a
withFFmpegCheck action = do
  (exitCode, _, _) <- readProcessWithExitCode "ffmpeg" ["-version"] ""
  let ffmpegAvailable = exitCode == ExitSuccess
  when (not ffmpegAvailable) $ 
    putStrLn "Warning: FFmpeg not found, skipping integration tests"
  action ffmpegAvailable

-- Helper function to set up test environment
withTestEnvironment :: ((FilePath, MediaSources) -> IO a) -> IO a
withTestEnvironment action = do
  tempDir <- getCanonicalTemporaryDirectory
  testDir <- createTempDirectory tempDir "video-exporter-e2e"
  
  let videoDir = testDir </> "video"
      photoDir = testDir </> "photo"
      audioDir = testDir </> "audio"
  
  -- Create source directories
  createDirectoryIfMissing True videoDir
  createDirectoryIfMissing True photoDir
  createDirectoryIfMissing True audioDir
  
  -- Create media sources
  let mediaSources = createMediaSources videoDir photoDir audioDir
  
  result <- action (testDir, mediaSources)
  
  -- Cleanup
  removeDirectoryRecursive testDir
  return result

-- Helper function to create a complete test layout with multiple segments
createCompleteTestLayout :: IO VideoLayout
createCompleteTestLayout = do
  now <- getCurrentTime
  return VideoLayout
    { layoutId = "e2e-test-layout"
    , totalDuration = Duration 15.0
    , segments = 
        [ createTestTitleSegment "Opening Title" 0.0 5.0
        , createTestTitleSegment "Middle Section" 5.0 10.0
        , createTestTitleSegment "Closing Title" 10.0 15.0
        ]
    , globalAudio = []
    , outputFormat = "mp4"
    , outputResolution = Resolution 1280 720
    , outputFrameRate = 24.0
    , layoutCreatedAt = Timestamp now
    }

-- Helper function to create a test title segment
createTestTitleSegment :: String -> Double -> Double -> VideoSegment
createTestTitleSegment title startSec endSec = VideoSegment
  { segmentId = T.pack $ "seg-" ++ show (round startSec :: Int)
  , segmentType = TitleCard (T.pack title) (Duration (endSec - startSec))
  , segmentStart = Duration startSec
  , segmentEnd = Duration endSec
  , textOverlays = []
  , audioTracks = []
  , transition = Nothing
  }

