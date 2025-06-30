{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FFmpeg.Config
  ( FFmpegConfig
  ) where

import GHC.Generics (Generic)

data FFmpegConfig = FFmpegConfig
  { ffmpegBinary    :: FilePath    -- ^ Path to ffmpeg binary
  , ffprobeBinary   :: FilePath    -- ^ Path to ffprobe binary  
  , tempDir         :: FilePath    -- ^ Directory for temporary files
  , maxConcurrency  :: Int         -- ^ Maximum concurrent processes
  , timeoutSeconds  :: Int         -- ^ Process timeout in seconds
  , verboseLogging  :: Bool        -- ^ Enable verbose logging
  } deriving (Show, Eq, Generic)