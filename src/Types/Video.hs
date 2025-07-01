{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Video
  ( VideoLayout (..)
  , VideoSegment (..)
  , SegmentType (..)
  , Transition (..)
  , AudioTrack (..)
  , TextOverlay (..)
  , MediaReference (..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Common (Duration(..), Resolution, Timestamp)

-- | Reference to a media file within a segment
data MediaReference = MediaReference
  { mediaId     :: Text -- reference to original media file
  , startTime   :: Duration -- start time within the media
  , endTime     :: Duration -- end time within the media
  , playbackSpeed :: Maybe Double -- 1.0 = normal speed
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

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
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Audio track information
data AudioTrack = AudioTrack
  { audioSource :: Text -- file path or music library reference
  , audioStart  :: Duration
  , audioEnd    :: Duration
  , volume      :: Double -- 0.0 to 1.0
  , fadeIn      :: Maybe Duration
  , fadeOut     :: Maybe Duration
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Individual video segment in the layout
data VideoSegment = VideoSegment
  { segmentId      :: Text
  , segmentType    :: SegmentType
  , segmentStart   :: Duration -- start time in final video
  , segmentEnd     :: Duration -- end time in final video
  , textOverlays   :: [TextOverlay]
  , audioTracks    :: [AudioTrack]
  , transition     :: Maybe Transition
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

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
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Custom JSON instances to handle flat structure with "type" field
instance ToJSON Transition where
  toJSON Cut = object ["type" .= ("Cut" :: Text)]
  toJSON (Fade duration) = object ["type" .= ("Fade" :: Text), "duration" .= duration]
  toJSON (Dissolve duration) = object ["type" .= ("Dissolve" :: Text), "duration" .= duration]
  toJSON (Wipe wipeType duration) = object ["type" .= ("Wipe" :: Text), "wipeType" .= wipeType, "duration" .= duration]
  toJSON (CustomTransition name duration) = object ["type" .= ("CustomTransition" :: Text), "name" .= name, "duration" .= duration]

instance FromJSON Transition where
  parseJSON = withObject "Transition" $ \o -> do
    t <- o .: "type"
    case (t :: Text) of
      "Cut" -> return Cut
      "Fade" -> Fade <$> o .: "duration"
      "Dissolve" -> Dissolve <$> o .: "duration" 
      "Wipe" -> Wipe <$> o .: "wipeType" <*> o .: "duration"
      "CustomTransition" -> CustomTransition <$> o .: "name" <*> o .: "duration"
      _ -> fail $ "Unknown transition type: " ++ show t

instance ToJSON SegmentType where
  toJSON (VideoClip ref) = object 
    [ "type" .= ("VideoClip" :: Text)
    , "mediaId" .= mediaId ref
    , "startTime" .= startTime ref
    , "endTime" .= endTime ref
    , "playbackSpeed" .= playbackSpeed ref
    ]
  toJSON (PhotoClip ref duration) = object
    [ "type" .= ("PhotoClip" :: Text)
    , "mediaId" .= mediaId ref
    , "duration" .= duration
    ]
  toJSON (TitleCard text duration) = object
    [ "type" .= ("TitleCard" :: Text)
    , "text" .= text
    , "duration" .= duration
    ]
  toJSON (TransitionSegment transition) = object
    [ "type" .= ("TransitionSegment" :: Text)
    , "transition" .= transition
    ]

instance FromJSON SegmentType where
  parseJSON = withObject "SegmentType" $ \o -> do
    t <- o .: "type"
    case (t :: Text) of
      "VideoClip" -> do
        mediaRef <- MediaReference 
          <$> o .: "mediaId"
          <*> o .: "startTime"
          <*> o .: "endTime"
          <*> o .: "playbackSpeed"
        return $ VideoClip mediaRef
      "PhotoClip" -> do
        mediaRef <- MediaReference
          <$> o .: "mediaId"
          <*> pure (Duration 0) -- PhotoClip doesn't use startTime/endTime from MediaReference
          <*> pure (Duration 0)
          <*> pure Nothing
        duration <- o .: "duration"
        return $ PhotoClip mediaRef duration
      "TitleCard" -> TitleCard <$> o .: "text" <*> o .: "duration"
      "TransitionSegment" -> TransitionSegment <$> o .: "transition"
      _ -> fail $ "Unknown segment type: " ++ show t

