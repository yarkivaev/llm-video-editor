{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module FFmpeg.Transcoder
  ( 
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Text as T
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Types.Transcoder
import Types.Video (VideoLayout(..), VideoSegment(..), SegmentType(..), MediaReference(..))
import Types.Render (OutputFile(..), MediaSources(..))
import Types.Common (Duration(..))
import FFmpeg.Config (FFmpegConfig(..))
import File
import FileSystem (MonadFileShow(..))
import Absolute.Common (AbsoluteFS(..), runAbsoluteFS)
import Absolute.Instances ()  -- for MonadFileShow instance
import System.FilePath ((</>))
import qualified Data.Text as T

-- | FFmpeg implementation of Transcoder
instance (Monad m, MonadIO m, MonadReader FFmpegConfig m) => Transcoder m where
  transcode request = do
    config <- ask
    liftIO $ do
      let tempDirPath = tempDir config
      createDirectoryIfMissing True tempDirPath
      cmd <- runAbsoluteFS $ generateFFmpegCommand config request
      let OutputFile outputPathFile = transcodeOutputPath request
      case cmd of
        [] -> return $ TranscodeFailure $ TranscodeInputError "Empty command"
        (_:args) -> return $ TranscodeSuccess $ TranscodeCommand (ffmpegBinary config) args outputPathFile

-- | Generate FFmpeg command based on layout
generateFFmpegCommand :: (MonadFileShow m, MonadIO m) => FFmpegConfig -> TranscodeRequest -> m [String]
generateFFmpegCommand config request = do 
  let layout = transcodeLayout request
      sources = transcodeMediaSources request
      OutputFile outputPathFile = transcodeOutputPath request
      segmentCount = length (segments layout)
      Duration layoutDuration = totalDuration layout
  
  let ffmpegPath = ffmpegBinary config
  outputPathStr <- case outputPathFile of
    File (Path []) fname -> return fname  -- For empty path, just use filename
    _ -> showFile outputPathFile          -- For non-empty path, use full path
  
  -- Generate input arguments for each segment
  inputArgs <- if segmentCount == 0 
              then return ["-f", "lavfi", "-i", "color=black:size=1920x1080:duration=1"] 
              else concat <$> mapM (getInputArgs sources) (zip [0..] (segments layout))
  
  -- Generate filter or duration arguments
  let durationArgs = if segmentCount <= 1
                     then ["-t", show layoutDuration]  -- Use total duration for single input
                     else []  -- Multi-segment handling would need concatenation
      
      filterComplex = if segmentCount == 0
                      then ["-filter_complex", "[0:v]scale=1920:1080[outv]", "-map", "[outv]"]
                      else if segmentCount == 1
                           then []  -- No filter complex needed for single input
                           else ["-filter_complex", generateFilterString segmentCount, "-map", "[outv]", "-map", "[outa]"]
  
  return $ [ ffmpegPath, "-y"
           , "-loglevel", if verboseLogging config then "info" else "warning"
           ] ++ inputArgs ++ filterComplex ++ durationArgs ++
           [ "-c:v", "libx264", "-preset", "medium"
           , "-crf", "23", "-pix_fmt", "yuv420p", T.unpack outputPathStr
           ]

-- | Get input arguments for a specific segment
getInputArgs :: MonadFileShow m => MediaSources -> (Int, VideoSegment) -> m [String]
getInputArgs sources (_, segment) = do 
  case segmentType segment of
    VideoClip mediaRef -> do
      let Duration startOffset = startTime mediaRef
          Duration endOffset = endTime mediaRef
          clipDuration = endOffset - startOffset
          fileName = T.unpack $ mediaId mediaRef
          videoDir = videoSourceDir sources
      dirPath <- showPath videoDir
      let filePath = T.unpack dirPath </> fileName
          seekArgs = if startOffset > 0 then ["-ss", show startOffset] else []
          durationArgs = ["-t", show clipDuration]
      return $ seekArgs ++ durationArgs ++ ["-i", filePath]
    PhotoClip mediaRef duration -> do
      let Duration photoDuration = duration
          fileName = T.unpack $ mediaId mediaRef
          photoDir = photoSourceDir sources
      dirPath <- showPath photoDir
      let filePath = T.unpack dirPath </> fileName
      return $ ["-loop", "1", "-t", show photoDuration, "-i", filePath]
    TitleCard _ duration -> do
      let Duration titleDuration = duration
      return $ ["-f", "lavfi", "-i", "color=black:size=1920x1080:duration=" ++ show titleDuration]
    TransitionSegment _ -> return [] -- Transitions don't need separate inputs



-- | Generate filter string for concatenating multiple segments
generateFilterString :: Int -> String
generateFilterString segCount = 
  let -- Scale all video inputs to consistent resolution and SAR
      videoFilters = map (\i -> "[" ++ show i ++ ":v]scale=1920:1080,setsar=1[v" ++ show i ++ "]") [0..segCount-1]
      -- Prepare audio inputs (use silent audio if no audio stream)
      audioFilters = map (\i -> "[" ++ show i ++ ":a]aresample=48000[a" ++ show i ++ "]") [0..segCount-1]
      -- All individual filters separated by semicolons
      allFilters = videoFilters ++ audioFilters
      -- Concatenate streams - FFmpeg concat filter expects: [v0][a0][v1][a1]...
      streamPairs = concatMap (\i -> ["[v" ++ show i ++ "]", "[a" ++ show i ++ "]"]) [0..segCount-1]
      concatInputs = concat streamPairs
      concatFilter = concatInputs ++ "concat=n=" ++ show segCount ++ ":v=1:a=1[outv][outa]"
  in intercalate ";" allFilters ++ ";" ++ concatFilter