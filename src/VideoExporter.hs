{-# LANGUAGE OverloadedStrings #-}

module VideoExporter
  ( -- * Main Export Functions
    exportVideo
  , exportVideoFromFile
  -- * Configuration
  , ExportConfig (..)
  , defaultExportConfig
  -- * Helper Functions
  , createMediaSources
  , createOutputPath
  , validateExportConfig
  , getSupportedFormats
  , isSupportedFormat
  -- * Re-exports
  , module VideoRenderer
  , module VideoRenderer.FFmpeg
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeExtension)

import Types.Video (VideoLayout)
import VideoRenderer
import VideoRenderer.FFmpeg

-- | Configuration for video export
data ExportConfig = ExportConfig
  { exportFFmpegConfig :: FFmpegConfig
  , exportMediaSources :: MediaSources
  , exportRenderOptions :: RenderOptions
  } deriving (Show, Eq)

-- | Default export configuration
defaultExportConfig :: MediaSources -> ExportConfig
defaultExportConfig mediaSources = ExportConfig
  { exportFFmpegConfig = defaultFFmpegConfig
  , exportMediaSources = mediaSources
  , exportRenderOptions = RenderOptions
    { quality = Nothing
    , codec = Nothing
    , bitrate = Nothing
    , audioCodec = Nothing
    , audioBitrate = Nothing
    , overwrite = False
    }
  }

-- | Export a video layout to a file using FFmpeg
exportVideo :: VideoLayout      -- ^ Video layout to render
           -> ExportConfig      -- ^ Export configuration  
           -> OutputPath        -- ^ Output file path
           -> IO RenderResult   -- ^ IO action that creates the video
exportVideo layout config outputPath = do
  let context = RenderContext
        { mediaSources = exportMediaSources config
        , outputPath = outputPath
        , renderOptions = exportRenderOptions config
        }
  
  runFFmpegRenderer (exportFFmpegConfig config) $ do
    renderVideo layout context

-- | Export a video layout from a JSON file
exportVideoFromFile :: FilePath         -- ^ Path to VideoLayout JSON file
                   -> ExportConfig      -- ^ Export configuration
                   -> OutputPath        -- ^ Output file path  
                   -> IO (Either String RenderResult)  -- ^ IO action result
exportVideoFromFile layoutFile config outputPath = do
  -- Check if layout file exists
  fileExists <- doesFileExist layoutFile
  if not fileExists
    then return $ Left $ "Layout file not found: " ++ layoutFile
    else do
      -- Parse the layout file
      parseResult <- parseVideoLayout layoutFile
      case parseResult of
        Left parseError -> return $ Left $ "Failed to parse layout file: " ++ parseError
        Right layout -> do
          -- Export the video
          renderResult <- exportVideo layout config outputPath
          return $ Right renderResult

-- | Helper function to create MediaSources from directory paths
createMediaSources :: FilePath  -- ^ Video source directory
                  -> FilePath  -- ^ Photo source directory  
                  -> FilePath  -- ^ Audio source directory
                  -> MediaSources
createMediaSources = MediaSources

-- | Helper function to create OutputPath
createOutputPath :: FilePath -> OutputPath
createOutputPath = OutputPath

-- | Validate export configuration
validateExportConfig :: ExportConfig -> IO (Either Text ())
validateExportConfig config = do
  let MediaSources videoDir photoDir audioDir = exportMediaSources config
  
  -- Check if source directories exist
  videoDirExists <- doesDirectoryExist videoDir
  photoDirExists <- doesDirectoryExist photoDir
  audioDirExists <- doesDirectoryExist audioDir
  
  if not videoDirExists
    then return $ Left $ "Video source directory does not exist: " <> T.pack videoDir
    else if not photoDirExists
      then return $ Left $ "Photo source directory does not exist: " <> T.pack photoDir
      else if not audioDirExists
        then return $ Left $ "Audio source directory does not exist: " <> T.pack audioDir
        else return $ Right ()

-- | Get supported output formats
getSupportedFormats :: [Text]
getSupportedFormats = ["mp4", "mov", "avi", "mkv", "webm"]

-- | Check if output format is supported
isSupportedFormat :: FilePath -> Bool
isSupportedFormat filePath = 
  let ext = T.pack $ drop 1 $ takeExtension filePath  -- remove the dot
  in ext `elem` getSupportedFormats