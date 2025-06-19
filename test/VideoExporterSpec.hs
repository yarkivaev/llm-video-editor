{-# LANGUAGE OverloadedStrings #-}

module VideoExporterSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Time (getCurrentTime)

import Types.Common
import Types.Video
import VideoExporter

spec :: Spec
spec = describe "VideoExporter" $ do
  
  describe "createMediaSources" $ do
    it "creates MediaSources with correct paths" $ do
      let sources = createMediaSources "/video" "/photo" "/audio"
      videoSourceDir sources `shouldBe` "/video"
      photoSourceDir sources `shouldBe` "/photo"
      audioSourceDir sources `shouldBe` "/audio"
  
  describe "createOutputPath" $ do
    it "creates OutputPath correctly" $ do
      let OutputPath path = createOutputPath "/output/video.mp4"
      path `shouldBe` "/output/video.mp4"
  
  describe "getSupportedFormats" $ do
    it "returns expected video formats" $ do
      let formats = getSupportedFormats
      formats `shouldContain` ["mp4"]
      formats `shouldContain` ["mov"]
      formats `shouldContain` ["avi"]
  
  describe "isSupportedFormat" $ do
    it "returns True for supported formats" $ do
      isSupportedFormat "video.mp4" `shouldBe` True
      isSupportedFormat "video.mov" `shouldBe` True
      isSupportedFormat "video.avi" `shouldBe` True
    
    it "returns False for unsupported formats" $ do
      isSupportedFormat "video.xyz" `shouldBe` False
      isSupportedFormat "video.doc" `shouldBe` False
  
  describe "defaultExportConfig" $ do
    it "creates config with provided media sources" $ do
      let sources = createMediaSources "/v" "/p" "/a"
          config = defaultExportConfig sources
      exportMediaSources config `shouldBe` sources
      exportRenderOptions config `shouldBe` defaultRenderOptions
  
  describe "validateExportConfig" $ around withTempDirs $ do
    it "validates config with existing directories" $ \(videoDir, photoDir, audioDir) -> do
      -- Verify directories were actually created
      videoDirExists <- doesDirectoryExist videoDir
      photoDirExists <- doesDirectoryExist photoDir
      audioDirExists <- doesDirectoryExist audioDir
      
      videoDirExists `shouldBe` True
      photoDirExists `shouldBe` True
      audioDirExists `shouldBe` True
      
      let sources = createMediaSources videoDir photoDir audioDir
          config = defaultExportConfig sources
      result <- validateExportConfig config
      result `shouldBe` Right ()
    
    it "fails validation with non-existing video directory" $ \(_, photoDir, audioDir) -> do
      let sources = createMediaSources "/nonexistent" photoDir audioDir
          config = defaultExportConfig sources
      result <- validateExportConfig config
      case result of
        Left err -> T.isInfixOf "Video source directory does not exist" err `shouldBe` True
        Right _ -> expectationFailure "Expected validation to fail"

-- Helper function to create temporary directories for testing
withTempDirs :: ((FilePath, FilePath, FilePath) -> IO a) -> IO a
withTempDirs action = do
  tempDir <- getCanonicalTemporaryDirectory
  baseDir <- createTempDirectory tempDir "video-exporter-test"
  
  let videoDir = baseDir </> "video"
      photoDir = baseDir </> "photo"  
      audioDir = baseDir </> "audio"
  
  createDirectoryIfMissing True videoDir
  createDirectoryIfMissing True photoDir
  createDirectoryIfMissing True audioDir
  
  -- Force filesystem sync to ensure directories are created
  videoDirExists <- doesDirectoryExist videoDir
  photoDirExists <- doesDirectoryExist photoDir
  audioDirExists <- doesDirectoryExist audioDir
  
  if not (videoDirExists && photoDirExists && audioDirExists)
    then error $ "Failed to create test directories: " ++ show (videoDirExists, photoDirExists, audioDirExists)
    else return ()
  
  result <- action (videoDir, photoDir, audioDir)
  
  removeDirectoryRecursive baseDir
  return result

-- Helper function to create a sample VideoLayout for testing
createSampleVideoLayout :: IO VideoLayout
createSampleVideoLayout = do
  now <- getCurrentTime
  return VideoLayout
    { layoutId = "test-layout"
    , totalDuration = Duration 60.0
    , segments = [createSampleSegment]
    , globalAudio = []
    , outputFormat = "mp4"
    , outputResolution = Resolution 1920 1080
    , outputFrameRate = 30.0
    , layoutCreatedAt = Timestamp now
    }

-- Helper function to create a sample VideoSegment
createSampleSegment :: VideoSegment
createSampleSegment = VideoSegment
  { segmentId = "seg-1"
  , segmentType = TitleCard "Test Title" (Duration 5.0)
  , segmentStart = Duration 0.0
  , segmentEnd = Duration 5.0
  , textOverlays = []
  , audioTracks = []
  , transition = Nothing
  }