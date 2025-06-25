{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Json.Video where

import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Types.Video
import Types.Common (Duration(..), Timestamp(..))
import Json.Common () -- Import instances for common types

instance ToJSON MediaReference
instance FromJSON MediaReference where
  parseJSON = withObject "MediaReference" $ \o -> MediaReference
    <$> o .: "mediaId"
    <*> (Duration <$> o .: "startTime")
    <*> (Duration <$> o .: "endTime")
    <*> o .:? "playbackSpeed"

instance ToJSON TextOverlay
instance FromJSON TextOverlay where
  parseJSON = withObject "TextOverlay" $ \o -> TextOverlay
    <$> o .: "overlayText"
    <*> o .: "overlayPosition"
    <*> (Duration <$> o .: "overlayDuration")
    <*> o .:? "overlayStyle"

instance ToJSON AudioTrack
instance FromJSON AudioTrack where
  parseJSON = withObject "AudioTrack" $ \o -> AudioTrack
    <$> o .: "audioSource"
    <*> (Duration <$> o .: "audioStart")
    <*> (Duration <$> o .: "audioEnd")
    <*> o .: "volume"
    <*> (fmap Duration <$> o .:? "fadeIn")
    <*> (fmap Duration <$> o .:? "fadeOut")

instance ToJSON VideoSegment
instance FromJSON VideoSegment where
  parseJSON = withObject "VideoSegment" $ \o -> VideoSegment
    <$> o .: "segmentId"
    <*> o .: "segmentType"
    <*> (Duration <$> o .: "segmentStart")
    <*> (Duration <$> o .: "segmentEnd")
    <*> o .: "textOverlays"
    <*> o .: "audioTracks"
    <*> o .:? "transition"

instance ToJSON VideoLayout
instance FromJSON VideoLayout where
  parseJSON = withObject "VideoLayout" $ \o -> do
    layoutId <- o .: "layoutId"
    totalDuration <- Duration <$> o .: "totalDuration"
    segments <- o .: "segments"
    globalAudio <- o .: "globalAudio"
    outputFormat <- o .: "outputFormat"
    outputResolution <- o .: "outputResolution"
    outputFrameRate <- o .: "outputFrameRate"
    layoutCreatedAt <- Timestamp <$> o .: "layoutCreatedAt"
    return VideoLayout{..}

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

instance FromJSON SegmentType where
  parseJSON = withObject "SegmentType" $ \o -> do
    segType <- o .: "type"
    case segType of
      "VideoClip" -> VideoClip <$> parseJSON (Object o)
      "PhotoClip" -> PhotoClip 
        <$> (do -- Create MediaReference for photo clip
              mediaId <- o .: "mediaId"
              return $ MediaReference mediaId (Duration 0.0) (Duration 0.0) Nothing)
        <*> (Duration <$> o .: "duration")
      "TitleCard" -> TitleCard
        <$> o .: "text"
        <*> (Duration <$> o .: "duration")
      _ -> fail $ "Unknown segment type: " ++ segType

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

instance FromJSON Transition where
  parseJSON value = case value of
    -- Handle null input for Cut transitions
    Null -> pure Cut
    -- Handle proper object input
    Object o -> do
      transType <- o .: "type"
      case transType of
        "Cut" -> pure Cut
        "Fade" -> Fade . Duration <$> o .: "duration"
        "Dissolve" -> Dissolve . Duration <$> o .: "duration"
        "Wipe" -> Wipe <$> o .: "wipeType" <*> (Duration <$> o .: "duration")
        "CustomTransition" -> CustomTransition <$> o .: "name" <*> (Duration <$> o .: "duration")
        _ -> fail $ "Unknown transition type: " ++ transType
    _ -> fail "Expected Object or null for Transition"