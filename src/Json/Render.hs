{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Json.Render where

import Data.Aeson
import Types.Render
import Json.Common ()


instance ToJSON MediaSources
instance FromJSON MediaSources
instance ToJSON OutputPath
instance FromJSON OutputPath
instance ToJSON RenderContext
instance FromJSON RenderContext
instance ToJSON RenderError
instance FromJSON RenderError
instance ToJSON RenderResult
instance FromJSON RenderResult