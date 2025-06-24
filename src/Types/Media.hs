{-# LANGUAGE DeriveGeneric #-}

module Types.Media
  ( MediaFile (..)
  , VideoFile (..)
  , PhotoFile (..)
  , MediaMetadata (..)
  , VideoContentAnalysis (..)
  , TimeBoundDetail (..)
  , UserPrompt
  , VideoRequest (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Common (Duration, Resolution, Location, Timestamp)

-- | Common metadata for media files
data MediaMetadata = MediaMetadata
  { fileName     :: Text
  , filePath     :: FilePath
  , fileSize     :: Int -- bytes
  , createdAt    :: Timestamp
  , location     :: Maybe Location
  , description  :: Maybe Text
  , tags         :: [Text]
  } deriving (Show, Eq, Generic)

-- | Video content analysis from neural network
data VideoContentAnalysis = VideoContentAnalysis
  { contentOverview     :: Text -- general description of video content
  , actionIntroduction  :: Text -- introduction to main actions/events
  , timeBoundDetails    :: [TimeBoundDetail] -- detailed time-based descriptions
  , detectedObjects     :: [Text] -- objects detected in video
  , detectedScenes      :: [Text] -- scene types (indoor, outdoor, etc.)
  , estimatedMood       :: Maybe Text -- mood/atmosphere of video
  } deriving (Show, Eq, Generic)

-- | Time-bound detail for specific moments in video
data TimeBoundDetail = TimeBoundDetail
  { detailStartTime :: Duration
  , detailEndTime   :: Duration
  , detailDescription :: Text
  , detailConfidence :: Maybe Double -- confidence score from 0.0 to 1.0
  } deriving (Show, Eq, Generic)

-- | Video file with metadata and content analysis
data VideoFile = VideoFile
  { videoMetadata :: MediaMetadata
  , videoDuration :: Duration
  , resolution    :: Resolution
  , frameRate     :: Double
  , hasAudio      :: Bool
  , videoFormat   :: Text -- e.g., "mp4", "mov", "avi"
  , contentAnalysis :: Maybe VideoContentAnalysis -- analysis from neural network
  } deriving (Show, Eq, Generic)

-- | Photo file with metadata
data PhotoFile = PhotoFile
  { photoMetadata :: MediaMetadata
  , photoResolution :: Resolution
  , imageFormat   :: Text -- e.g., "jpg", "png", "raw"
  , cameraSettings :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Union type for all media files
data MediaFile
  = Video VideoFile
  | Photo PhotoFile
  deriving (Show, Eq, Generic)

-- | User prompt - raw text that goes directly to LLM
type UserPrompt = Text

-- | Complete video creation request
data VideoRequest = VideoRequest
  { requestId    :: Text
  , mediaFiles   :: [MediaFile]
  , userPrompt   :: UserPrompt
  , submittedAt  :: Timestamp
  } deriving (Show, Eq, Generic)