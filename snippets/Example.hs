{-# LANGUAGE OverloadedStrings #-}

module Example where

-- Example snippets for testing llm-video-editor modules
-- Import any modules you need from the main project

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale, getCurrentTime)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Types.Common
import Types.Media
import Types.Video
import VideoExporter
import VideoRenderer (singleMediaSources)


videoSegment0 :: VideoSegment
videoSegment0 = VideoSegment
  { segmentId = T.pack "test-title-seg"
  , segmentType = VideoClip $ 
      MediaReference (T.pack "stream.mp4") (Duration 0) (Duration 2) Nothing
  , segmentStart = Duration 0.0
  , segmentEnd = Duration 2.0
  , textOverlays = []
  , audioTracks = []
  , transition = Nothing
  }

videoSegment :: VideoSegment
videoSegment = VideoSegment
  { segmentId = T.pack "test-title-seg"
  , segmentType = VideoClip $ 
      MediaReference (T.pack "video2.mp4") (Duration 2) (Duration 12) Nothing
  , segmentStart = Duration 2.0
  , segmentEnd = Duration 12.0
  , textOverlays = []
  , audioTracks = []
  , transition = Nothing
  }

videoLayout :: IO VideoLayout
videoLayout = do
  now <- getCurrentTime
  return VideoLayout
    { layoutId = "integration-test-layout"
    , totalDuration = Duration 12.0  -- 10 second video
    , segments = [videoSegment0, videoSegment]
    , globalAudio = []
    , outputFormat = "mp4"
    , outputResolution = Resolution 1280 720  -- 720p for faster rendering
    , outputFrameRate = 24.0  -- 24fps for faster rendering  
    , layoutCreatedAt = Timestamp now
    }

exportConfig :: IO ExportConfig
exportConfig = do
  workDir <- getCurrentDirectory
  return $ defaultExportConfig $ singleMediaSources $ workDir </> "examples"

path :: IO OutputPath
path = getCurrentDirectory >>= \workDir ->  return $ OutputPath (workDir </> "output.mp4")

renderResult :: IO RenderResult
renderResult = do
  layout <- videoLayout
  config <- exportConfig
  _path <- path
  exportVideo layout config _path