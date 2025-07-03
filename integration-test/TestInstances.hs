{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module TestInstances where

import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Trans (lift)
import FFmpeg.Config (FFmpegConfig(..))
import Relative.Common (RelativeFS(..))
import FileSystem (MonadFileShow(..))
import File (File, Path)

-- Instance to allow ReaderT FFmpegConfig RelativeFS to work
instance MonadReader FFmpegConfig RelativeFS where
  ask = return $ FFmpegConfig 
    { ffmpegBinary = "/usr/bin/ffmpeg"
    , ffprobeBinary = "/usr/bin/ffprobe"
    , tempDir = "/tmp"
    , timeoutSeconds = 60
    , verboseLogging = False
    , maxConcurrency = 1
    }
  local _ m = m

-- Instance for MonadFileShow (ReaderT FFmpegConfig RelativeFS)
instance MonadFileShow RelativeFS => MonadFileShow (ReaderT FFmpegConfig RelativeFS) where
  showPath path = lift $ showPath path
  showFile file = lift $ showFile file