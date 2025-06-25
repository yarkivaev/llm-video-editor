{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Json.System where

import Data.Aeson
import Types.System
import Json.Common () -- Import instances for common types
import Json.Media () -- Import instances for media types
import Json.Video () -- Import instances for video types

instance ToJSON ProcessingStatus
instance FromJSON ProcessingStatus

instance ToJSON VideoEditorError
instance FromJSON VideoEditorError

instance ToJSON VideoEditorInput
instance FromJSON VideoEditorInput

instance ToJSON VideoEditorOutput
instance FromJSON VideoEditorOutput