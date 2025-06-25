{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Json.Common where

import Data.Aeson
import Types.Common

instance ToJSON Duration
instance FromJSON Duration

instance ToJSON Resolution
instance FromJSON Resolution where
  parseJSON = withObject "Resolution" $ \o -> Resolution
    <$> o .: "width"
    <*> o .: "height"

instance ToJSON Location
instance FromJSON Location

instance ToJSON Timestamp
instance FromJSON Timestamp
