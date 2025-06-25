{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Json.Media where

import Data.Aeson
import Types.Media
import Types.Common
import Json.Common ()

import Data.Aeson.Types (Parser)
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)

instance ToJSON MediaMetadata
instance FromJSON MediaMetadata where
  parseJSON = withObject "MediaMetadata" $ \o -> MediaMetadata
    <$> o .: "fileName"
    <*> o .: "filePath"
    <*> o .: "fileSize"
    <*> (Timestamp <$> (o .: "createdAt" >>= parseTimeString))
    <*> o .:? "location"
    <*> o .:? "description"
    <*> o .:? "tags" .!= []
    where
      parseTimeString :: String -> Parser UTCTime
      parseTimeString = pure . parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

instance ToJSON VideoContentAnalysis
instance FromJSON VideoContentAnalysis where
  parseJSON = withObject "VideoContentAnalysis" $ \o -> VideoContentAnalysis
    <$> o .: "contentOverview"
    <*> o .: "actionIntroduction"
    <*> o .: "timeBoundDetails"
    <*> o .: "detectedObjects"
    <*> o .: "detectedScenes"
    <*> o .:? "estimatedMood"

instance ToJSON TimeBoundDetail
instance FromJSON TimeBoundDetail where
  parseJSON = withObject "TimeBoundDetail" $ \o -> TimeBoundDetail
    <$> (Duration <$> o .: "detailStartTime")
    <*> (Duration <$> o .: "detailEndTime")
    <*> o .: "detailDescription"
    <*> o .:? "detailConfidence"

instance ToJSON VideoFile
instance FromJSON VideoFile where
  parseJSON = withObject "VideoFile" $ \o -> VideoFile
    <$> o .: "metadata"
    <*> (Duration <$> o .: "duration")
    <*> o .: "resolution"
    <*> o .: "frameRate"
    <*> o .: "hasAudio"
    <*> o .: "videoFormat"
    <*> o .:? "contentAnalysis"

instance ToJSON PhotoFile
instance FromJSON PhotoFile where
  parseJSON = withObject "PhotoFile" $ \o -> PhotoFile
    <$> o .: "metadata"
    <*> o .: "resolution"
    <*> o .: "imageFormat"
    <*> o .:? "cameraSettings"

instance ToJSON MediaFile
instance FromJSON MediaFile where
  parseJSON = withObject "MediaFile" $ \o -> do
    fileType <- o .: "type"
    case fileType of
      "video" -> Video <$> parseJSON (Object o)
      "photo" -> Photo <$> parseJSON (Object o)
      _ -> fail $ "Unknown media file type: " ++ fileType

instance ToJSON VideoRequest
instance FromJSON VideoRequest