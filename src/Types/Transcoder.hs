{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Transcoder
  ( Transcoder(..)
  , TranscodeRequest(..)
  , TranscodeResult(..)
  , TranscodeError(..)
  , TranscodeCommand(..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import Types.Video (VideoLayout(..), VideoSegment(..), SegmentType(..))
import Types.Render (MediaSources(..), OutputPath(..))
import FFmpeg.Config (FFmpegConfig(..))

-- | Transcoding request containing all necessary information
data TranscodeRequest = TranscodeRequest
  { transcodeLayout :: VideoLayout      -- ^ The video layout to transcode
  , transcodeMediaSources :: MediaSources -- ^ Source directories for media files
  , transcodeOutputPath :: OutputPath   -- ^ Output file path
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Errors that can occur during transcoding
data TranscodeError
  = TranscodeInputError Text            -- ^ Invalid input parameters
  | TranscodeProcessError Text          -- ^ Error during transcoding process
  | TranscodeOutputError Text           -- ^ Error creating output file
  | TranscodeFileNotFound FilePath      -- ^ Required file not found
  | TranscodeTimeout                    -- ^ Transcoding timed out
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Command to be executed for transcoding
data TranscodeCommand = TranscodeCommand
  { commandBinary :: FilePath           -- ^ Executable to run
  , commandArgs :: [String]             -- ^ Arguments to pass
  , commandOutputPath :: FilePath       -- ^ Expected output file path
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Result of transcoding operation
data TranscodeResult
  = TranscodeSuccess TranscodeCommand   -- ^ Command to execute
  | TranscodeFailure TranscodeError     -- ^ Error during preparation
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Typeclass for video transcoding implementations
class Monad m => Transcoder m where
  -- | Prepare transcode command for a video layout
  transcode :: TranscodeRequest -> m TranscodeResult