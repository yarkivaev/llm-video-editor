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

import Data.Aeson (FromJSON, ToJSON(..), object, (.=), Key)
import Data.Aeson.Key (fromString)
import Data.Text (Text)
import qualified Data.Text as T
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

-- JSON instances (ToJSON only, FromJSON instances are in VideoAssembler.LLM to avoid orphan warnings)
instance ToJSON MediaReference
instance ToJSON TextOverlay
instance ToJSON AudioTrack
instance ToJSON VideoSegment
instance ToJSON VideoLayout

-- Custom ToJSON instances to match the format expected by LLM FromJSON instances
instance ToJSON SegmentType where
  toJSON (VideoClip mediaRef) = object
    [ fromString "type" .= T.pack "VideoClip"
    , fromString "mediaId" .= mediaId mediaRef
    , fromString "startTime" .= startTime mediaRef
    , fromString "endTime" .= endTime mediaRef
    , fromString "playbackSpeed" .= playbackSpeed mediaRef
    ]
  toJSON (PhotoClip mediaRef duration) = object
    [ fromString "type" .= T.pack "PhotoClip"
    , fromString "mediaId" .= mediaId mediaRef
    , fromString "duration" .= duration
    ]
  toJSON (TitleCard text duration) = object
    [ fromString "type" .= T.pack "TitleCard"
    , fromString "text" .= text
    , fromString "duration" .= duration
    ]
  toJSON (TransitionSegment transition) = object
    [ fromString "type" .= T.pack "TransitionSegment"
    , fromString "transition" .= transition
    ]

instance ToJSON Transition where
  toJSON Cut = object [fromString "type" .= T.pack "Cut"]
  toJSON (Fade duration) = object
    [ fromString "type" .= T.pack "Fade"
    , fromString "duration" .= duration
    ]
  toJSON (Dissolve duration) = object
    [ fromString "type" .= T.pack "Dissolve"
    , fromString "duration" .= duration
    ]
  toJSON (Wipe wipeType duration) = object
    [ fromString "type" .= T.pack "Wipe"
    , fromString "wipeType" .= wipeType
    , fromString "duration" .= duration
    ]
  toJSON (CustomTransition name duration) = object
    [ fromString "type" .= T.pack "CustomTransition"
    , fromString "name" .= name
    , fromString "duration" .= duration
    ]