{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VideoRenderer
  ( -- * Types
    VideoRenderer (..)
  , RenderContext (..)
  , RenderResult (..)
  , RenderError (..)
  , MediaSources (..)
  , singleMediaSources
  , OutputPath (..)
  , RenderOptions (..)
  , defaultRenderOptions
  -- * Parsing
  , parseVideoLayout
  ) where

import Control.Exception (try, IOException)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import Types.Video (VideoLayout)
-- Import to make FromJSON VideoLayout instance available
import VideoAssembler.LLM ()

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

-- | Rendering options
data RenderOptions = RenderOptions
  { quality      :: Maybe Text     -- ^ Quality preset (e.g., "high", "medium", "low")
  , codec        :: Maybe Text     -- ^ Video codec (e.g., "h264", "h265")
  , bitrate      :: Maybe Text     -- ^ Video bitrate (e.g., "5000k")
  , audioCodec   :: Maybe Text     -- ^ Audio codec (e.g., "aac", "mp3")
  , audioBitrate :: Maybe Text     -- ^ Audio bitrate (e.g., "192k")
  , overwrite    :: Bool           -- ^ Whether to overwrite existing output file
  } deriving (Show, Eq, Generic)

-- | Default rendering options
defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions
  { quality = Nothing
  , codec = Nothing
  , bitrate = Nothing
  , audioCodec = Nothing
  , audioBitrate = Nothing
  , overwrite = False
  }

-- | Context for video rendering
data RenderContext = RenderContext
  { mediaSources   :: MediaSources
  , outputPath     :: OutputPath
  , renderOptions  :: RenderOptions
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
  
  -- | Validate render context and video layout before rendering
  validateRender :: VideoLayout -> RenderContext -> m (Either RenderError ())
  validateRender _layout _context = pure (Right ()) -- default: always valid
  
  -- | Estimate rendering time
  estimateRenderTime :: VideoLayout -> RenderContext -> m Double
  estimateRenderTime _layout _context = pure 60.0 -- default: 60 seconds
  
  -- | Get renderer capabilities
  getRendererCapabilities :: m [Text]
  getRendererCapabilities = pure [] -- default: no specific capabilities

-- | Parse VideoLayout from JSON file
parseVideoLayout :: FilePath -> IO (Either String VideoLayout)
parseVideoLayout filePath = do
  result <- try $ eitherDecodeFileStrict filePath
  case result of
    Left (e :: IOException) -> return $ Left $ "File not found: " ++ filePath ++ " (" ++ show e ++ ")"
    Right parseResult -> return parseResult

-- JSON instances
instance ToJSON MediaSources
instance FromJSON MediaSources
instance ToJSON OutputPath
instance FromJSON OutputPath
instance ToJSON RenderOptions
instance FromJSON RenderOptions
instance ToJSON RenderContext
instance FromJSON RenderContext
instance ToJSON RenderError
instance FromJSON RenderError
instance ToJSON RenderResult
instance FromJSON RenderResult