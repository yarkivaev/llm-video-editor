{-# LANGUAGE DeriveGeneric #-}

module Types.Video
  ( VideoLayout (..)
  , VideoSegment (..)
  , SegmentType (..)
  , Transition (..)
  , AudioTrack (..)
  , TextOverlay (..)
  , MediaReference (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Common (Duration, Resolution, Timestamp)

-- | Reference to a media file within a segment
data MediaReference = MediaReference
  { mediaId     :: Text -- reference to original media file
  , startTime   :: Duration -- start time within the media
  , endTime     :: Duration -- end time within the media
  , playbackSpeed :: Maybe Double -- 1.0 = normal speed
  } deriving (Show, Eq, Generic)

-- | Transition between segments
data Transition
  = Cut
  | Fade Duration
  | Dissolve Duration
  | Wipe Text Duration -- wipe type and duration
  | CustomTransition Text Duration
  deriving (Show, Eq, Generic)

-- | Type of video segment
data SegmentType
  = VideoClip MediaReference
  | PhotoClip MediaReference Duration -- photo + display duration
  | TitleCard Text Duration
  | TransitionSegment Transition
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

