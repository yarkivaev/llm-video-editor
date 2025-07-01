{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module FFmpeg.Transcoder
  ( 
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import System.Directory (createDirectoryIfMissing)
import Types.Transcoder
import Types.Video (VideoLayout(..))
import Types.Render (OutputPath(..))
import FFmpeg.Config (FFmpegConfig(..))

-- | FFmpeg implementation of Transcoder
instance (Monad m, MonadIO m, MonadReader FFmpegConfig m) => Transcoder m where
  transcode request = do
    config <- ask
    liftIO $ do
      createDirectoryIfMissing True (tempDir config)
      let cmd = generateFFmpegCommand config request
          OutputPath outputPath' = transcodeOutputPath request
      case cmd of
        [] -> return $ TranscodeFailure $ TranscodeInputError "Empty command"
        (binary:args) -> return $ TranscodeSuccess $ TranscodeCommand binary args outputPath'

-- | Generate simple FFmpeg command
generateFFmpegCommand :: FFmpegConfig -> TranscodeRequest -> [String]
generateFFmpegCommand config request = 
  let OutputPath outputPath' = transcodeOutputPath request
  in [ ffmpegBinary config, "-y"
     , "-loglevel", if verboseLogging config then "info" else "warning"
     , "-i", "input.mp4"
     , "-c:v", "libx264", "-preset", "medium"
     , "-crf", "23", "-pix_fmt", "yuv420p", outputPath'
     ]