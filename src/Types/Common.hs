{-# LANGUAGE DeriveGeneric #-}

module Types.Common
  ( Duration (..)
  , Resolution (..)
  , Location (..)
  , Timestamp (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
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

-- JSON instances
instance ToJSON Duration
instance FromJSON Duration
instance ToJSON Resolution
instance ToJSON Location
instance FromJSON Location
instance ToJSON Timestamp
instance FromJSON Timestamp