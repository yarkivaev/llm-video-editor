{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.System
  ( VideoEditorInput (..)
  , VideoEditorOutput (..)
  , ProcessingStatus (..)
  , VideoEditorError (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Common (Duration)
import Types.Media (VideoRequest)
import Types.Video (VideoLayout)

-- | Processing status for video generation
data ProcessingStatus
  = Pending
  | Processing Double -- progress percentage (0.0 to 1.0)
  | Completed
  | Failed VideoEditorError
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Error types for video editor
data VideoEditorError
  = InvalidMediaFile Text
  | UnsupportedFormat Text
  | InsufficientMedia Text
  | ProcessingError Text
  | LLMError Text
  | SystemError Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Input to the video editor system
data VideoEditorInput = VideoEditorInput
  { inputRequest :: VideoRequest
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Output from the video editor system
data VideoEditorOutput = VideoEditorOutput
  { outputLayout :: VideoLayout
  , processingStatus :: ProcessingStatus
  , estimatedRenderTime :: Maybe Duration
  , warnings :: [Text]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)