{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( -- * Media Types
    MediaFile (..)
  , VideoFile (..)
  , PhotoFile (..)
  , MediaMetadata (..)
  , VideoContentAnalysis (..)
  , TimeBoundDetail (..)
  , Duration (..)
  , Resolution (..)
  , Location (..)
  , Timestamp (..)
    -- * User Input Types
  , UserPrompt
  , VideoRequest (..)
    -- * Video Layout Types
  , VideoLayout (..)
  , VideoSegment (..)
  , SegmentType (..)
  , Transition (..)
  , AudioTrack (..)
  , TextOverlay (..)
  , MediaReference (..)
    -- * System Types
  , VideoEditorInput (..)
  , VideoEditorOutput (..)
  , ProcessingStatus (..)
  , VideoEditorError (..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Duration in seconds
newtype Duration = Duration Double
  deriving (Show, Eq, Ord, Generic)

-- | Video resolution
data Resolution = Resolution
  { width  :: Int
  , height :: Int
  } deriving (Show, Eq, Generic)

-- | Geographic location metadata
data Location = Location
  { latitude  :: Double
  , longitude :: Double
  , address   :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Timestamp for media files
newtype Timestamp = Timestamp UTCTime
  deriving (Show, Eq, Ord, Generic)

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

-- | Reference to a media file within a segment
data MediaReference = MediaReference
  { mediaId     :: Text -- reference to original media file
  , startTime   :: Duration -- start time within the media
  , endTime     :: Duration -- end time within the media
  , playbackSpeed :: Maybe Double -- 1.0 = normal speed
  } deriving (Show, Eq, Generic)

-- | Type of video segment
data SegmentType
  = VideoClip MediaReference
  | PhotoClip MediaReference Duration -- photo + display duration
  | TitleCard Text Duration
  | TransitionSegment Transition
  deriving (Show, Eq, Generic)

-- | Transition between segments
data Transition
  = Cut
  | Fade Duration
  | Dissolve Duration
  | Wipe Text Duration -- wipe type and duration
  | CustomTransition Text Duration
  deriving (Show, Eq, Generic)

-- | Text overlay for segments
data TextOverlay = TextOverlay
  { overlayText     :: Text
  , overlayPosition :: (Double, Double) -- x, y coordinates (0-1 normalized)
  , overlayDuration :: Duration
  , overlayStyle    :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Audio track information
data AudioTrack = AudioTrack
  { audioSource :: Text -- file path or music library reference
  , audioStart  :: Duration
  , audioEnd    :: Duration
  , volume      :: Double -- 0.0 to 1.0
  , fadeIn      :: Maybe Duration
  , fadeOut     :: Maybe Duration
  } deriving (Show, Eq, Generic)

-- | Individual video segment in the layout
data VideoSegment = VideoSegment
  { segmentId      :: Text
  , segmentType    :: SegmentType
  , segmentStart   :: Duration -- start time in final video
  , segmentEnd     :: Duration -- end time in final video
  , textOverlays   :: [TextOverlay]
  , audioTracks    :: [AudioTrack]
  , transition     :: Maybe Transition
  } deriving (Show, Eq, Generic)

-- | Complete video layout plan
data VideoLayout = VideoLayout
  { layoutId         :: Text
  , totalDuration    :: Duration
  , segments         :: [VideoSegment]
  , globalAudio      :: [AudioTrack] -- background music, etc.
  , outputFormat     :: Text -- e.g., "mp4", "mov"
  , outputResolution :: Resolution
  , outputFrameRate  :: Double
  , layoutCreatedAt  :: Timestamp
  } deriving (Show, Eq, Generic)

-- | Processing status for video generation
data ProcessingStatus
  = Pending
  | Processing Double -- progress percentage (0.0 to 1.0)
  | Completed
  | Failed VideoEditorError
  deriving (Show, Eq, Generic)

-- | Error types for video editor
data VideoEditorError
  = InvalidMediaFile Text
  | UnsupportedFormat Text
  | InsufficientMedia Text
  | ProcessingError Text
  | LLMError Text
  | SystemError Text
  deriving (Show, Eq, Generic)

-- | Input to the video editor system
data VideoEditorInput = VideoEditorInput
  { inputRequest :: VideoRequest
  } deriving (Show, Eq, Generic)

-- | Output from the video editor system
data VideoEditorOutput = VideoEditorOutput
  { outputLayout :: VideoLayout
  , processingStatus :: ProcessingStatus
  , estimatedRenderTime :: Maybe Duration
  , warnings :: [Text]
  } deriving (Show, Eq, Generic)