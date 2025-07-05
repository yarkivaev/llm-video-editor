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
import Data.List (intercalate)
import Types.Transcoder
import Types.Video (VideoLayout(..), VideoSegment(..), SegmentType(..), MediaReference(..))
import Types.Render (OutputFile(..), MediaSources(..))
import Types.Common (Duration(..))
import FFmpeg.Config (FFmpegConfig(..))
import File
import FileSystem (MonadFileShow(..))
import Relative.Common ()
import Relative.Instances ()  -- for MonadFileShow instance
import System.FilePath ((</>))
-- | FFmpeg implementation of Transcoder
instance (Monad m, MonadIO m, MonadFileShow m, MonadReader FFmpegConfig m) => Transcoder m where
  transcode request = do
    config <- ask
    let tempDirPath = tempDir config
    liftIO $ createDirectoryIfMissing True tempDirPath
    cmd <- generateFFmpegCommand config request
    let OutputFile outputPathFile = transcodeOutputPath request
    case cmd of
      [] -> return $ TranscodeFailure $ TranscodeInputError "Empty command"
      (_:args) -> return $ TranscodeSuccess $ TranscodeCommand (ffmpegBinary config) args outputPathFile

-- | Generate FFmpeg command based on layout
generateFFmpegCommand :: MonadFileShow m => FFmpegConfig -> TranscodeRequest -> m [String]
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
                           else ["-filter_complex", generateFilterStringWithAudio (segments layout), "-map", "[outv]", "-map", "[outa]"]
  
  let audioArgs = ["-c:a", "aac", "-b:a", "128k"]  -- Always preserve/generate audio
  
  return $ [ ffmpegPath, "-y"
           , "-loglevel", if verboseLogging config then "info" else "warning"
           ] ++ inputArgs ++ filterComplex ++ durationArgs ++
           [ "-c:v", "libx264", "-preset", "medium"
           , "-crf", "23", "-pix_fmt", "yuv420p"
           ] ++ audioArgs ++ [T.unpack outputPathStr]

-- | Get input arguments for a specific segment
getInputArgs :: MonadFileShow m => MediaSources -> (Int, VideoSegment) -> m [String]
getInputArgs sources (_, segment) = do 
  case segmentType segment of
    VideoClip mediaRef -> do
      let Duration startOffset = startTime mediaRef
          Duration endOffset = endTime mediaRef
          clipDuration = endOffset - startOffset
          fileNameStr = T.unpack $ mediaId mediaRef
          videoDir = videoSourceDir sources
      dirPath <- showPath videoDir
      let mediaFilePath = T.unpack dirPath </> fileNameStr
          seekArgs = if startOffset > 0 then ["-ss", show startOffset] else []
          durationArgs = ["-t", show clipDuration]
      return $ seekArgs ++ durationArgs ++ ["-i", mediaFilePath]
    PhotoClip mediaRef duration -> do
      let Duration photoDuration = duration
          fileNameStr = T.unpack $ mediaId mediaRef
          photoDir = photoSourceDir sources
      dirPath <- showPath photoDir
      let mediaFilePath = T.unpack dirPath </> fileNameStr
      return $ ["-framerate", "30", "-loop", "1", "-t", show photoDuration, "-i", mediaFilePath]
    TitleCard _ duration -> do
      let Duration titleDuration = duration
      return $ ["-f", "lavfi", "-i", "color=black:size=1920x1080:duration=" ++ show titleDuration]
    TransitionSegment _ -> return [] -- Transitions don't need separate inputs



-- | Generate filter string for concatenating multiple segments with audio
generateFilterStringWithAudio :: [VideoSegment] -> String
generateFilterStringWithAudio segments =
  let segCount = length segments
      -- Scale all video inputs to consistent resolution and SAR
      videoFilters = map (\i -> "[" ++ show i ++ ":v]scale=1920:1080,setsar=1[v" ++ show i ++ "]") [0..segCount-1]
      -- For audio, use original streams when available, generate silent audio for others
      audioFilters = zipWith generateAudioFilter [0..] segments
      -- Prepare video and audio inputs for concatenation
      videoInputs = map (\i -> "[v" ++ show i ++ "]") [0..segCount-1]
      audioInputs = map (\i -> "[a" ++ show i ++ "]") [0..segCount-1]
      -- Create properly ordered inputs: v0,a0,v1,a1,v2,a2...
      interleavedInputs = concat $ zipWith (\v a -> v ++ a) videoInputs audioInputs
      concatFilter = interleavedInputs ++ "concat=n=" ++ show segCount ++ ":v=1:a=1[outv][outa]"
      allFilters = videoFilters ++ audioFilters ++ [concatFilter]
  in intercalate ";" allFilters

-- | Generate audio filter for a specific segment based on its type  
generateAudioFilter :: Int -> VideoSegment -> String
generateAudioFilter i segment = case segmentType segment of
  VideoClip _ -> 
    -- For video clips, use original audio directly (no padding needed - video timing controls duration)
    "[" ++ show i ++ ":a]aresample=48000[a" ++ show i ++ "]"
  PhotoClip _ duration ->
    -- For photo clips, generate silent audio for the duration
    let Duration photoDuration = duration
    in "aevalsrc=0:channel_layout=stereo:sample_rate=48000:duration=" ++ show photoDuration ++ "[a" ++ show i ++ "]"
  TitleCard _ duration ->
    -- For title cards, generate silent audio for the duration  
    let Duration titleDuration = duration
    in "aevalsrc=0:channel_layout=stereo:sample_rate=48000:duration=" ++ show titleDuration ++ "[a" ++ show i ++ "]"
  TransitionSegment _ -> "anullsrc=channel_layout=stereo:sample_rate=48000[a" ++ show i ++ "]"

-- | Generate filter string for concatenating multiple segments (legacy, video only)
generateFilterString :: Int -> String
generateFilterString segCount = 
  let -- Scale all video inputs to consistent resolution and SAR
      videoFilters = map (\i -> "[" ++ show i ++ ":v]scale=1920:1080,setsar=1[v" ++ show i ++ "]") [0..segCount-1]
      -- Concatenate video streams only - skip audio for now to avoid the mixing issue
      videoInputs = map (\i -> "[v" ++ show i ++ "]") [0..segCount-1]
      concatFilter = concat videoInputs ++ "concat=n=" ++ show segCount ++ ":v=1:a=0[outv]"
  in intercalate ";" videoFilters ++ ";" ++ concatFilter