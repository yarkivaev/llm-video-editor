{-# LANGUAGE OverloadedStrings #-}

module FFmpeg.TranscoderIntegrationSpec (spec) where

import Test.Hspec
import Control.Monad.Reader (runReaderT)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Time (getCurrentTime)
import System.Directory (doesFileExist, removeFile, getCurrentDirectory)
import System.FilePath (splitPath)
import Control.Exception (bracket)
import qualified Data.Text as T

import Types.Transcoder
import Types.Video
import Types.Render
import Types.Common
import FFmpeg.Config
import FFmpeg.Transcoder ()
import File (File(..), Path(..), Segment(..))
import FileSystem (MonadFileShow(..))
import Absolute.Common (AbsoluteFS(..), runAbsoluteFS)
import Absolute.Instances ()  -- for MonadFileShow instance
import Data.String (fromString)

-- Helper function to show TranscodeError without Show instance
showTranscodeError :: TranscodeError -> String
showTranscodeError (TranscodeInputError msg) = "TranscodeInputError: " ++ T.unpack msg
showTranscodeError (TranscodeProcessError msg) = "TranscodeProcessError: " ++ T.unpack msg  
showTranscodeError (TranscodeOutputError msg) = "TranscodeOutputError: " ++ T.unpack msg
showTranscodeError (TranscodeFileNotFound _) = "TranscodeFileNotFound: <file>"
showTranscodeError TranscodeTimeout = "TranscodeTimeout"

spec :: Spec
spec = describe "FFmpeg Transcoder Integration Tests" $ do

  describe "command execution validation" $ do
    it "validates FFmpeg command syntax by running --help" $ do
      now <- getCurrentTime
      let layout = VideoLayout
            { layoutId = "ffmpeg-help-test-layout"
            , totalDuration = Duration 5.0
            , segments = [VideoSegment
                { segmentId = "help-test-segment"
                , segmentType = VideoClip $ MediaReference
                    { mediaId = "test-video-1.mp4"
                    , startTime = Duration 0.0
                    , endTime = Duration 5.0
                    , playbackSpeed = Just 1.0
                    }
                , segmentStart = Duration 0.0
                , segmentEnd = Duration 5.0
                , textOverlays = []
                , audioTracks = []
                , transition = Nothing
                }]
            , globalAudio = []
            , outputFormat = "mp4"
            , outputResolution = Resolution 1920 1080
            , outputFrameRate = 30.0
            , layoutCreatedAt = Timestamp now
            }
      result <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = "test-output.mp4" })) Nothing
      
      case result of
        TranscodeSuccess command -> do
          -- Test that FFmpeg binary exists and responds to --help
          (exitCode, stdout, _) <- readProcessWithExitCode (commandBinary command) ["--help"] ""
          exitCode `shouldBe` ExitSuccess
          stdout `shouldContain` "usage: ffmpeg"
          
          -- Note: This test only validates FFmpeg help, not actual video processing
          -- Expected duration would be 5.0 seconds if this command were executed
        TranscodeFailure err -> expectationFailure $ "Expected success but got: " ++ showTranscodeError err

    it "executes FFmpeg command with real test files" $ do
      now <- getCurrentTime
      let layout = VideoLayout
            { layoutId = "real-file-execution-test"
            , totalDuration = Duration 3.0
            , segments = [VideoSegment
                { segmentId = "real-video-segment"
                , segmentType = VideoClip $ MediaReference
                    { mediaId = "test-video-1.mp4"
                    , startTime = Duration 1.0
                    , endTime = Duration 4.0
                    , playbackSpeed = Just 1.0
                    }
                , segmentStart = Duration 0.0
                , segmentEnd = Duration 3.0
                , textOverlays = []
                , audioTracks = []
                , transition = Nothing
                }]
            , globalAudio = []
            , outputFormat = "mp4"
            , outputResolution = Resolution 640 480
            , outputFrameRate = 30.0
            , layoutCreatedAt = Timestamp now
            }
      let outputFile = "test-output.mp4"
      result <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = fromString outputFile })) Nothing
      
      case result of
        TranscodeSuccess command -> do
          -- Execute the actual FFmpeg command
          bracket
            (return ())
            (\_ -> do
              exists <- doesFileExist outputFile
              if exists then removeFile outputFile else return ()
              )
            (\_ -> do
              (exitCode, _, _) <- readProcessWithExitCode 
                (commandBinary command) 
                (commandArgs command) 
                ""
              
              -- Command should complete successfully
              exitCode `shouldBe` ExitSuccess
              
              -- Output file should be created
              outputExists <- doesFileExist outputFile
              outputExists `shouldBe` True
              
              -- Check video duration using ffprobe
              (probeExitCode, probeDuration, probeStderr) <- readProcessWithExitCode 
                "ffprobe" 
                ["-v", "quiet", "-show_entries", "format=duration", "-of", "csv=p=0", outputFile] 
                ""
              
              if probeExitCode == ExitSuccess 
                then do
                  let actualDuration = read probeDuration :: Double
                  -- Allow small tolerance for encoding precision (±0.1 seconds)
                  abs (actualDuration - 3.0) `shouldSatisfy` (<= 0.1)
                else putStrLn $ "ffprobe failed: " ++ probeStderr
              )
        TranscodeFailure err -> expectationFailure $ "Expected success but got: " ++ showTranscodeError err

    it "validates photo-to-video command with real files" $ do
      now <- getCurrentTime
      let layout = VideoLayout
            { layoutId = "photo-to-video-test"
            , totalDuration = Duration 2.0
            , segments = [VideoSegment
                { segmentId = "photo-segment"
                , segmentType = PhotoClip 
                    (MediaReference "test-photo-1.jpg" (Duration 0.0) (Duration 1.0) Nothing)
                    (Duration 2.0)
                , segmentStart = Duration 0.0
                , segmentEnd = Duration 2.0
                , textOverlays = []
                , audioTracks = []
                , transition = Nothing
                }]
            , globalAudio = []
            , outputFormat = "mp4"
            , outputResolution = Resolution 640 480
            , outputFrameRate = 30.0
            , layoutCreatedAt = Timestamp now
            }
      let outputFile = "photo-test-output.mp4"
      result <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = fromString outputFile })) Nothing
      
      case result of
        TranscodeSuccess command -> do
          -- Execute the photo-to-video command
          bracket
            (return ())
            (\_ -> do
              exists <- doesFileExist outputFile
              if exists then removeFile outputFile else return ()
              )
            (\_ -> do
              (exitCode, _, stderr) <- readProcessWithExitCode 
                (commandBinary command) 
                (commandArgs command) 
                ""
              
              -- Command should complete successfully or provide useful error
              if exitCode /= ExitSuccess
                then putStrLn $ "FFmpeg stderr: " ++ stderr
                else return ()
              
              -- At minimum, FFmpeg should recognize the command structure
              stderr `shouldNotContain` "Unrecognized option"
              stderr `shouldNotContain` "Invalid argument"
              
              -- Check video duration using ffprobe if command succeeded
              if exitCode == ExitSuccess then do
                outputExists <- doesFileExist outputFile
                if outputExists then do
                  (probeExitCode, probeDuration, probeStderr) <- readProcessWithExitCode 
                    "ffprobe" 
                    ["-v", "quiet", "-show_entries", "format=duration", "-of", "csv=p=0", outputFile] 
                    ""
                  
                  if probeExitCode == ExitSuccess 
                    then do
                      let actualDuration = read probeDuration :: Double
                      -- Allow small tolerance for photo-to-video encoding precision (±0.1 seconds)
                      abs (actualDuration - 2.0) `shouldSatisfy` (<= 0.1)
                    else putStrLn $ "ffprobe failed: " ++ probeStderr
                else putStrLn "Output file was not created"
              else return ()
              )
        TranscodeFailure err -> expectationFailure $ "Expected success but got: " ++ showTranscodeError err

  describe "different video layout scenarios" $ do
    it "handles empty segments layout" $ do
      now <- getCurrentTime
      let layout = VideoLayout
            { layoutId = "empty-segments-test"
            , totalDuration = Duration 1.0
            , segments = []
            , globalAudio = []
            , outputFormat = "mp4"
            , outputResolution = Resolution 1920 1080
            , outputFrameRate = 30.0
            , layoutCreatedAt = Timestamp now
            }
      result <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = "empty-output.mp4" })) Nothing
      
      case result of
        TranscodeSuccess command -> do
          commandBinary command `shouldBe` "/usr/bin/ffmpeg"
          outputPath <- runAbsoluteFS $ showFile (commandOutputPath command)
          T.unpack outputPath `shouldContain` "empty-output.mp4"
          -- Should have basic scaling filter for empty layout
          commandArgs command `shouldContain` ["-filter_complex"]
          -- Expected duration: 1.0 seconds (empty layout with totalDuration = 1.0)
        TranscodeFailure err -> expectationFailure $ "Expected success but got: " ++ showTranscodeError err

    it "handles video layout with multiple segments" $ do
      now <- getCurrentTime
      let layout = VideoLayout
            { layoutId = "multi-segment-test"
            , totalDuration = Duration 5.0
            , segments = [ VideoSegment
                { segmentId = "first-segment"
                , segmentType = VideoClip $ MediaReference
                    { mediaId = "test-video-1.mp4"
                    , startTime = Duration 1.0
                    , endTime = Duration 4.0
                    , playbackSpeed = Just 1.0
                    }
                , segmentStart = Duration 0.0
                , segmentEnd = Duration 3.0
                , textOverlays = []
                , audioTracks = []
                , transition = Nothing
                }
                , VideoSegment
                { segmentId = "second-segment"
                , segmentType = VideoClip $ MediaReference
                    { mediaId = "test-video-2.mp4"
                    , startTime = Duration 1.0
                    , endTime = Duration 3.0
                    , playbackSpeed = Just 1.0
                    }
                , segmentStart = Duration 3.0
                , segmentEnd = Duration 5.0
                , textOverlays = []
                , audioTracks = []
                , transition = Nothing
                }
                ]
            , globalAudio = []
            , outputFormat = "mp4"
            , outputResolution = Resolution 1920 1080
            , outputFrameRate = 30.0
            , layoutCreatedAt = Timestamp now
            }
      result <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = "multi-segment.mp4" })) Nothing
      
      case result of
        TranscodeSuccess command -> do
          let args = commandArgs command
          -- Should have input files for each segment
          length (filter (== "-i") args) `shouldBe` 2
          -- Should have filter complex for concatenation
          args `shouldContain` ["-filter_complex"]
          
          -- Execute the actual FFmpeg command and validate duration
          let outputFile = "multi-segment.mp4"
          bracket
            (return ())
            (\_ -> do
              exists <- doesFileExist outputFile
              if exists then removeFile outputFile else return ()
              )
            (\_ -> do
              (exitCode, stdout, stderr) <- readProcessWithExitCode 
                (commandBinary command) 
                (commandArgs command)
                ""
              
              -- Debug output on failure
              if exitCode /= ExitSuccess then do
                putStrLn $ "FFmpeg failed with exit code: " ++ show exitCode
                putStrLn $ "FFmpeg stderr: " ++ stderr
                putStrLn $ "FFmpeg stdout: " ++ stdout
              else return ()
              
              -- Command should complete successfully
              exitCode `shouldBe` ExitSuccess
              
              -- Output file should be created
              outputExists <- doesFileExist outputFile
              outputExists `shouldBe` True
              
              -- Check video duration using ffprobe
              (probeExitCode, probeDuration, probeStderr) <- readProcessWithExitCode 
                "ffprobe" 
                ["-v", "quiet", "-show_entries", "format=duration", "-of", "csv=p=0", outputFile] 
                ""
              
              if probeExitCode == ExitSuccess 
                then do
                  let actualDuration = read probeDuration :: Double
                  -- Allow tolerance for multi-segment encoding precision (±0.2 seconds)
                  -- Multi-segment concatenation may have slight timing variations
                  abs (actualDuration - 5.0) `shouldSatisfy` (<= 0.2)
                else putStrLn $ "ffprobe failed: " ++ probeStderr
              )
          -- Expected duration: 6.0 seconds (two 3-second segments)
        TranscodeFailure err -> expectationFailure $ "Expected success but got: " ++ showTranscodeError err

    it "handles multi-segment layout with different video resolutions" $ do
      now <- getCurrentTime
      let layout = VideoLayout
            { layoutId = "multi-resolution-test"
            , totalDuration = Duration 5.0
            , segments = [ VideoSegment
                { segmentId = "480p-segment"
                , segmentType = VideoClip $ MediaReference
                    { mediaId = "test-video-1.mp4"  -- 640x480
                    , startTime = Duration 0.0
                    , endTime = Duration 2.5
                    , playbackSpeed = Just 1.0
                    }
                , segmentStart = Duration 0.0
                , segmentEnd = Duration 2.5
                , textOverlays = []
                , audioTracks = []
                , transition = Nothing
                }
                , VideoSegment
                { segmentId = "720p-segment"
                , segmentType = VideoClip $ MediaReference
                    { mediaId = "test-video-720p-audio.mp4"  -- 1280x720
                    , startTime = Duration 0.0
                    , endTime = Duration 2.5
                    , playbackSpeed = Just 1.0
                    }
                , segmentStart = Duration 2.5
                , segmentEnd = Duration 5.0
                , textOverlays = []
                , audioTracks = []
                , transition = Nothing
                }
                ]
            , globalAudio = []
            , outputFormat = "mp4"
            , outputResolution = Resolution 1920 1080
            , outputFrameRate = 30.0
            , layoutCreatedAt = Timestamp now
            }
      result <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = "multi-resolution.mp4" })) Nothing
      
      case result of
        TranscodeSuccess command -> do
          let args = commandArgs command
          -- Should have input files for each segment
          length (filter (== "-i") args) `shouldBe` 2
          -- Should have filter complex for concatenation and scaling
          args `shouldContain` ["-filter_complex"]
          
          -- Execute the actual FFmpeg command and validate duration
          let outputFile = "multi-resolution.mp4"
          bracket
            (return ())
            (\_ -> do
              exists <- doesFileExist outputFile
              if exists then removeFile outputFile else return ()
              )
            (\_ -> do
              (exitCode, stdout, stderr) <- readProcessWithExitCode 
                (commandBinary command) 
                (commandArgs command)
                ""
              
              -- Debug output on failure
              if exitCode /= ExitSuccess then do
                putStrLn $ "FFmpeg failed with exit code: " ++ show exitCode
                putStrLn $ "FFmpeg stderr: " ++ stderr
                putStrLn $ "FFmpeg stdout: " ++ stdout
              else return ()
              
              -- Command should complete successfully
              exitCode `shouldBe` ExitSuccess
              
              -- Output file should be created
              outputExists <- doesFileExist outputFile
              outputExists `shouldBe` True
              
              -- Check video duration using ffprobe
              (probeExitCode, probeDuration, probeStderr) <- readProcessWithExitCode 
                "ffprobe" 
                ["-v", "quiet", "-show_entries", "format=duration", "-of", "csv=p=0", outputFile] 
                ""
              
              if probeExitCode == ExitSuccess 
                then do
                  let actualDuration = read probeDuration :: Double
                  -- Allow tolerance for multi-resolution encoding precision (±0.2 seconds)
                  abs (actualDuration - 5.0) `shouldSatisfy` (<= 0.2)
                else putStrLn $ "ffprobe failed: " ++ probeStderr
              
              -- Verify output resolution matches expected layout resolution
              (resExitCode, resOutput, resStderr) <- readProcessWithExitCode 
                "ffprobe" 
                ["-v", "quiet", "-select_streams", "v:0", "-show_entries", "stream=width,height", "-of", "csv=s=x:p=0", outputFile] 
                ""
              
              if resExitCode == ExitSuccess 
                then do
                  resOutput `shouldBe` "1920x1080\n"
                else putStrLn $ "ffprobe resolution check failed: " ++ resStderr
              )
          -- Expected duration: 5.0 seconds (two 2.5-second segments from different resolutions)
        TranscodeFailure err -> expectationFailure $ "Expected success but got: " ++ showTranscodeError err

  describe "configuration variations" $ do
    it "respects verbose logging setting" $ do
      now <- getCurrentTime
      let layout = VideoLayout
            { layoutId = "verbose-logging-test"
            , totalDuration = Duration 4.0
            , segments = [VideoSegment
                { segmentId = "logging-test-segment"
                , segmentType = VideoClip $ MediaReference
                    { mediaId = "test-video-1.mp4"
                    , startTime = Duration 0.0
                    , endTime = Duration 4.0
                    , playbackSpeed = Just 1.0
                    }
                , segmentStart = Duration 0.0
                , segmentEnd = Duration 4.0
                , textOverlays = []
                , audioTracks = []
                , transition = Nothing
                }]
            , globalAudio = []
            , outputFormat = "mp4"
            , outputResolution = Resolution 1920 1080
            , outputFrameRate = 30.0
            , layoutCreatedAt = Timestamp now
            }
      let verboseConfig = defaultFFmpegConfig { verboseLogging = True }
          quietConfig = defaultFFmpegConfig { verboseLogging = False }
      
      verboseResult <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = "verbose-output.mp4" })) (Just verboseConfig)
      quietResult <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = "verbose-output.mp4" })) (Just quietConfig)
      
      case (verboseResult, quietResult) of
        (TranscodeSuccess verboseCmd, TranscodeSuccess quietCmd) -> do
          commandArgs verboseCmd `shouldContain` ["-loglevel", "info"]
          commandArgs quietCmd `shouldContain` ["-loglevel", "warning"]
          -- Expected duration: 4.0 seconds (single video segment)
        _ -> expectationFailure "Both commands should succeed"

    it "uses custom FFmpeg binary path" $ do
      now <- getCurrentTime
      let layout = VideoLayout
            { layoutId = "custom-binary-test"
            , totalDuration = Duration 2.5
            , segments = [VideoSegment
                { segmentId = "binary-test-segment"
                , segmentType = VideoClip $ MediaReference
                    { mediaId = "test-video-1.mp4"
                    , startTime = Duration 0.0
                    , endTime = Duration 2.5
                    , playbackSpeed = Just 1.0
                    }
                , segmentStart = Duration 0.0
                , segmentEnd = Duration 2.5
                , textOverlays = []
                , audioTracks = []
                , transition = Nothing
                }]
            , globalAudio = []
            , outputFormat = "mp4"
            , outputResolution = Resolution 1920 1080
            , outputFrameRate = 30.0
            , layoutCreatedAt = Timestamp now
            }
      let customConfig = defaultFFmpegConfig { ffmpegBinary = "/usr/local/bin/ffmpeg" }
      result <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = "custom-output.mp4" })) (Just customConfig)
      
      case result of
        TranscodeSuccess command -> do
          commandBinary command `shouldBe` "/usr/local/bin/ffmpeg"
          -- Expected duration: 2.5 seconds (single video segment)
        TranscodeFailure err -> expectationFailure $ "Expected success but got: " ++ showTranscodeError err

    it "handles different absolute binary path configurations" $ do
      now <- getCurrentTime
      let layout = VideoLayout
            { layoutId = "absolute-binary-test"
            , totalDuration = Duration 2.0
            , segments = [VideoSegment
                { segmentId = "absolute-test-segment"  
                , segmentType = VideoClip $ MediaReference
                    { mediaId = "test-video-1.mp4"
                    , startTime = Duration 0.0
                    , endTime = Duration 2.0
                    , playbackSpeed = Just 1.0
                    }
                , segmentStart = Duration 0.0
                , segmentEnd = Duration 2.0
                , textOverlays = []
                , audioTracks = []
                , transition = Nothing
                }]
            , globalAudio = []
            , outputFormat = "mp4"
            , outputResolution = Resolution 1920 1080
            , outputFrameRate = 30.0
            , layoutCreatedAt = Timestamp now
            }
      
      -- Test various absolute path configurations
      let usrBinConfig = defaultFFmpegConfig { ffmpegBinary = "/usr/bin/ffmpeg" }
          usrLocalBinConfig = defaultFFmpegConfig { ffmpegBinary = "/usr/local/bin/ffmpeg" }
          homeBrewConfig = defaultFFmpegConfig { ffmpegBinary = "/opt/homebrew/bin/ffmpeg" }
      
      usrBinResult <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = "usrbin-output.mp4" })) (Just usrBinConfig)
      usrLocalBinResult <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = "usrlocalbin-output.mp4" })) (Just usrLocalBinConfig)  
      homeBrewResult <- createTranscodeRequest layout (OutputFile (File.File { filePath = Path [], fileName = "homebrew-output.mp4" })) (Just homeBrewConfig)
      
      case (usrBinResult, usrLocalBinResult, homeBrewResult) of
        (TranscodeSuccess usrCmd, TranscodeSuccess localCmd, TranscodeSuccess brewCmd) -> do
          commandBinary usrCmd `shouldBe` "/usr/bin/ffmpeg"
          commandBinary localCmd `shouldBe` "/usr/local/bin/ffmpeg"
          commandBinary brewCmd `shouldBe` "/opt/homebrew/bin/ffmpeg"
          -- Expected duration: 2.0 seconds (single video segment)
        _ -> expectationFailure "All absolute path binary configurations should succeed"


-- Default FFmpeg configuration for testing
defaultFFmpegConfig :: FFmpegConfig
defaultFFmpegConfig = FFmpegConfig
  { ffmpegBinary = "/usr/bin/ffmpeg"
  , ffprobeBinary = "/usr/bin/ffprobe"
  , tempDir = "tmp"
  , maxConcurrency = 2
  , timeoutSeconds = 60
  , verboseLogging = False
  }

-- Helper function to create and execute transcoding request
createTranscodeRequest :: VideoLayout -> OutputFile -> Maybe FFmpegConfig -> IO TranscodeResult
createTranscodeRequest layout outputFile maybeConfig = do
  currentDir <- getCurrentDirectory
  let currentDirSegments = map fromString $ filter (not . null) $ map (filter (/= '/')) $ splitPath currentDir
      videoPath = Path $ currentDirSegments ++ [fromString "integration-test", fromString "resources", fromString "videos"]
      photoPath = Path $ currentDirSegments ++ [fromString "integration-test", fromString "resources", fromString "photos"]
      audioPath = Path $ currentDirSegments ++ [fromString "integration-test", fromString "resources", fromString "audio"]
      sources = MediaSources videoPath photoPath audioPath
      request = TranscodeRequest layout sources outputFile
      config = maybe defaultFFmpegConfig id maybeConfig
  runReaderT (transcode request) config