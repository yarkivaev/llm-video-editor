{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Common (Duration, Resolution, Location, Timestamp)
import File

-- | Common metadata for media files
data MediaMetadata = MediaMetadata
  { file         :: File
  , fileSize     :: Int -- bytes
  , createdAt    :: Timestamp
  , location     :: Maybe Location
  , description  :: Maybe Text
  , tags         :: [Text]
  } deriving (Eq, Generic, FromJSON, ToJSON)

-- | Video content analysis from neural network
data VideoContentAnalysis = VideoContentAnalysis
  { contentOverview     :: Text -- general description of video content
  , actionIntroduction  :: Text -- introduction to main actions/events
  , timeBoundDetails    :: [TimeBoundDetail] -- detailed time-based descriptions
  , detectedObjects     :: [Text] -- objects detected in video
  , detectedScenes      :: [Text] -- scene types (indoor, outdoor, etc.)
  , estimatedMood       :: Maybe Text -- mood/atmosphere of video
  } deriving (Eq, Generic, FromJSON, ToJSON)

-- | Time-bound detail for specific moments in video
data TimeBoundDetail = TimeBoundDetail
  { detailStartTime :: Duration
  , detailEndTime   :: Duration
  , detailDescription :: Text
  , detailConfidence :: Maybe Double -- confidence score from 0.0 to 1.0
  } deriving (Eq, Generic, FromJSON, ToJSON)

-- | Video file with metadata and content analysis
data VideoFile = VideoFile
  { videoMetadata :: MediaMetadata
  , videoDuration :: Duration
  , resolution    :: Resolution
  , frameRate     :: Double
  , hasAudio      :: Bool
  , videoFormat   :: Text -- e.g., "mp4", "mov", "avi"
  , contentAnalysis :: Maybe VideoContentAnalysis -- analysis from neural network
  } deriving (Eq, Generic, FromJSON, ToJSON)

-- | Photo file with metadata
data PhotoFile = PhotoFile
  { photoMetadata :: MediaMetadata
  , photoResolution :: Resolution
  , imageFormat   :: Text -- e.g., "jpg", "png", "raw"
  , cameraSettings :: Maybe Text
  } deriving (Eq, Generic, FromJSON, ToJSON)

-- | Union type for all media files
data MediaFile
  = Video VideoFile
  | Photo PhotoFile
  deriving (Eq, Generic, FromJSON, ToJSON)

-- | User prompt - raw text that goes directly to LLMApi
type UserPrompt = Text

-- | Complete video creation request
data VideoRequest = VideoRequest
  { requestId    :: Text
  , mediaFiles   :: [MediaFile]
  , userPrompt   :: UserPrompt
  , submittedAt  :: Timestamp
  } deriving (Eq, Generic, FromJSON, ToJSON)