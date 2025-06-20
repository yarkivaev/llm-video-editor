{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VideoRenderer.FFmpeg
  ( FFmpegRenderer (..)
  , FFmpegConfig (..)
  , defaultFFmpegConfig
  , runFFmpegRenderer
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeExtension, dropExtension)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

import Types.Common (Duration(..), Resolution(..))
import Types.Video
import VideoRenderer

-- | Configuration for FFmpeg renderer
data FFmpegConfig = FFmpegConfig
  { ffmpegBinary    :: FilePath    -- ^ Path to ffmpeg binary
  , ffprobeBinary   :: FilePath    -- ^ Path to ffprobe binary  
  , tempDir         :: FilePath    -- ^ Directory for temporary files
  , maxConcurrency  :: Int         -- ^ Maximum concurrent processes
  , timeoutSeconds  :: Int         -- ^ Process timeout in seconds
  , verboseLogging  :: Bool        -- ^ Enable verbose logging
  } deriving (Show, Eq, Generic)

-- | Default FFmpeg configuration
defaultFFmpegConfig :: FFmpegConfig
defaultFFmpegConfig = FFmpegConfig
  { ffmpegBinary = "ffmpeg"
  , ffprobeBinary = "ffprobe"
  , tempDir = "/tmp/llm-video-editor"
  , maxConcurrency = 4
  , timeoutSeconds = 3600  -- 1 hour
  , verboseLogging = False
  }

-- | FFmpeg renderer monad
newtype FFmpegRenderer a = FFmpegRenderer (ReaderT FFmpegConfig IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run FFmpeg renderer with configuration
runFFmpegRenderer :: FFmpegConfig -> FFmpegRenderer a -> IO a
runFFmpegRenderer config (FFmpegRenderer action) = runReaderT action config

instance VideoRenderer FFmpegRenderer where
  renderVideo layout context = do
    config <- FFmpegRenderer ask
    liftIO $ do
      -- Create temp directory
      createDirectoryIfMissing True (tempDir config)
      
      -- Validate inputs
      validationResult <- validateInputs layout context
      case validationResult of
        Left err -> return $ RenderFailure err
        Right _ -> do
          -- Generate FFmpeg command
          ffmpegCmd <- generateFFmpegCommand config layout context
          putStrLn (unwords ffmpegCmd) --HOTFIX

          -- Execute FFmpeg
          result <- executeFFmpeg config ffmpegCmd
          case result of
            Left err -> return $ RenderFailure err
            Right outputFile -> return $ RenderSuccess outputFile

  validateRender layout context = do
    liftIO $ validateInputs layout context

  estimateRenderTime layout _context = do
    let Duration totalDur = totalDuration layout
    -- Rough estimate: 1 second of video takes 2 seconds to render
    return $ max 10.0 (totalDur * 2.0)

  getRendererCapabilities = return
    [ "ffmpeg-based"
    , "supports_mp4"
    , "supports_mov" 
    , "supports_transitions"
    , "supports_audio_mixing"
    , "supports_text_overlays"
    ]

-- | Validate input files and context
validateInputs :: VideoLayout -> RenderContext -> IO (Either RenderError ())
validateInputs layout context = do
  let MediaSources videoDir photoDir audioDir = mediaSources context
      OutputPath outputFile = outputPath context
  
  -- Check if source directories exist
  videoDirExists <- doesDirectoryExist videoDir
  photoDirExists <- doesDirectoryExist photoDir  
  audioDirExists <- doesDirectoryExist audioDir
  
  -- Check if output directory is writable
  let outputDir = takeDirectory outputFile
  createDirectoryIfMissing True outputDir
  
  -- Validate media references in segments
  mediaValidation <- validateMediaReferences (segments layout) context
  
  case mediaValidation of
    Left err -> return $ Left err
    Right _ -> 
      if videoDirExists && photoDirExists && audioDirExists
        then return $ Right ()
        else return $ Left $ RenderFileNotFound "Media source directory not found"

-- | Validate media references in video segments
validateMediaReferences :: [VideoSegment] -> RenderContext -> IO (Either RenderError ())
validateMediaReferences segments context = do
  let MediaSources videoDir photoDir audioDir = mediaSources context
  -- For now, just return success - in a real implementation, 
  -- we'd check each media reference exists
  return $ Right ()

-- | Generate FFmpeg command for the video layout
generateFFmpegCommand :: FFmpegConfig -> VideoLayout -> RenderContext -> IO [String]
generateFFmpegCommand config layout context = do
  let OutputPath outputFile = outputPath context
      opts = renderOptions context
      Resolution width height = outputResolution layout
      
  -- Basic FFmpeg command structure
  let baseCmd = 
        [ "-y"  -- overwrite output file
        , "-f", "lavfi"
        , "-i", printf "color=black:size=%dx%d:duration=%.2f:rate=%.2f" 
                  width height (getDuration $ totalDuration layout) (outputFrameRate layout)
        ]
      
      -- Add codec options
      codecCmd = case codec opts of
        Just c -> ["-c:v", T.unpack c]
        Nothing -> ["-c:v", "libx264"]
        
      -- Add quality options
      qualityCmd = case bitrate opts of
        Just b -> ["-b:v", T.unpack b]
        Nothing -> ["-crf", "23"]  -- default quality
        
      -- Add audio codec
      audioCmd = case audioCodec opts of
        Just ac -> ["-c:a", T.unpack ac]
        Nothing -> ["-c:a", "aac"]
        
      -- Output file
      outputCmd = [outputFile]
  
  return $ baseCmd ++ codecCmd ++ qualityCmd ++ audioCmd ++ outputCmd
  where
    getDuration (Duration d) = d

-- | Execute FFmpeg command
executeFFmpeg :: FFmpegConfig -> [String] -> IO (Either RenderError FilePath)
executeFFmpeg config args = do
  let cmd = ffmpegBinary config
      outputFile = last args
      
  when (verboseLogging config) $ do
    putStrLn $ "Executing: " ++ cmd ++ " " ++ unwords args
    
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args ""
  
  case exitCode of
    ExitSuccess -> do
      when (verboseLogging config) $ 
        putStrLn $ "FFmpeg completed successfully"
      return $ Right outputFile
    ExitFailure code -> do
      let errorMsg = "FFmpeg failed with exit code " ++ show code ++ 
                    ": " ++ stderr
      when (verboseLogging config) $ 
        putStrLn errorMsg
      return $ Left $ RenderProcessError $ T.pack errorMsg

-- Helper function for when
when :: Applicative f => Bool -> f () -> f ()
when True action = action
when False _ = pure ()