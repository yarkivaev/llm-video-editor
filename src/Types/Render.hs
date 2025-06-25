{-# LANGUAGE DeriveGeneric #-}

module Types.Render
  ( MediaSources (..)
  , singleMediaSources
  , OutputPath (..)
  , RenderContext (..)
  , RenderError (..)
  , RenderResult (..)
  , VideoRenderer (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Video (VideoLayout)

-- | Paths to media source directories
data MediaSources = MediaSources
  { videoSourceDir :: FilePath  -- ^ Directory containing video files
  , photoSourceDir :: FilePath  -- ^ Directory containing photo files
  , audioSourceDir :: FilePath  -- ^ Directory containing audio files
  } deriving (Show, Eq, Generic)

singleMediaSources :: FilePath -> MediaSources
singleMediaSources sourceDir = MediaSources sourceDir sourceDir sourceDir

-- | Output file path
newtype OutputPath = OutputPath FilePath
  deriving (Show, Eq, Generic)

-- | Context for video rendering
data RenderContext = RenderContext
  { mediaSources   :: MediaSources
  , outputPath     :: OutputPath
  } deriving (Show, Eq, Generic)

-- | Errors that can occur during rendering
data RenderError
  = RenderFileNotFound FilePath
  | RenderInvalidInput Text
  | RenderProcessError Text
  | RenderOutputError Text
  | RenderTimeoutError
  | RenderPermissionError FilePath
  deriving (Show, Eq, Generic)

-- | Result of video rendering
data RenderResult
  = RenderSuccess FilePath      -- ^ Path to created video file
  | RenderFailure RenderError   -- ^ Error during rendering
  deriving (Show, Eq, Generic)

-- | Typeclass for video rendering implementations
class Monad m => VideoRenderer m where
  -- | Render a video layout to a file
  renderVideo :: VideoLayout -> RenderContext -> m RenderResult
