{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Types.Render
  ( MediaSources (..)
  , singleMediaSources
  , OutputFile (..)
  , RenderContext (..)
  , RenderError (..)
  , RenderResult (..)
  , VideoRenderer (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Video (VideoLayout)
import File (File, Path)

-- | Paths to media source directories
data MediaSources = MediaSources
  { videoSourceDir :: Path  -- ^ Directory containing video files
  , photoSourceDir :: Path  -- ^ Directory containing photo files
  , audioSourceDir :: Path  -- ^ Directory containing audio files
  } deriving (Eq, Generic, FromJSON, ToJSON)

singleMediaSources :: Path -> MediaSources
singleMediaSources sourceDir = MediaSources sourceDir sourceDir sourceDir

-- | Output file path
newtype OutputFile = OutputFile File
  deriving (Eq, Generic, FromJSON, ToJSON)

-- | Context for video rendering
data RenderContext = RenderContext
  { mediaSources   :: MediaSources
  , outputPath     :: OutputFile
  } deriving (Eq, Generic, FromJSON, ToJSON)

-- | Errors that can occur during rendering
data RenderError
  = RenderFileNotFound File
  | RenderInvalidInput Text
  | RenderProcessError Text
  | RenderOutputError Text
  | RenderTimeoutError
  | RenderPermissionError File
  deriving (Eq, Generic, FromJSON, ToJSON)

-- | Result of video rendering
data RenderResult
  = RenderSuccess File      -- ^ Path to created video file
  | RenderFailure RenderError   -- ^ Error during rendering
  deriving (Eq, Generic, FromJSON, ToJSON)

-- | Typeclass for video rendering implementations
class Monad m => VideoRenderer m where
  -- | Render a video layout to a file
  renderVideo :: VideoLayout -> RenderContext -> m RenderResult
