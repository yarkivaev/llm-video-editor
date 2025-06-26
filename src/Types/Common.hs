{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

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
  deriving (FromJSON, ToJSON) via Double

-- | Video resolution
data Resolution = Resolution
  { width  :: Int
  , height :: Int
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Geographic location metadata
data Location = Location
  { latitude  :: Double
  , longitude :: Double
  , address   :: Maybe Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Timestamp for media files
newtype Timestamp = Timestamp UTCTime
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via UTCTime
