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
import System.FilePath ((</>))
import qualified Data.Text as T
import Data.List (intercalate)
import Types.Transcoder
import Types.Video (VideoLayout(..), VideoSegment(..), SegmentType(..), MediaReference(..))
import Types.Render (OutputPath(..), MediaSources(..))
import Types.Common (Duration(..))
import FFmpeg.Config (FFmpegConfig(..))

-- | FFmpeg implementation of Transcoder
instance (Monad m, MonadIO m, MonadReader FFmpegConfig m) => Transcoder m where
  transcode request = do
    config <- ask
    liftIO $ do
      createDirectoryIfMissing True (tempDir config)
      let cmd = generateFFmpegCommand config request
          OutputPath outputPath' = transcodeOutputPath request
      case cmd of
        [] -> return $ TranscodeFailure $ TranscodeInputError "Empty command"
        (binary:args) -> return $ TranscodeSuccess $ TranscodeCommand binary args outputPath'

-- | Generate FFmpeg command based on layout
generateFFmpegCommand :: FFmpegConfig -> TranscodeRequest -> [String]
generateFFmpegCommand config request = 
  let layout = transcodeLayout request
      sources = transcodeMediaSources request
      OutputPath outputPath' = transcodeOutputPath request
      segmentCount = length (segments layout)
      Duration layoutDuration = totalDuration layout
      
      -- Generate input arguments for each segment
      inputArgs = if segmentCount == 0 
                  then ["-f", "lavfi", "-i", "color=black:size=1920x1080:duration=1"] 
                  else concatMap (getInputArgs sources) (zip [0..] (segments layout))
      
      -- Generate filter or duration arguments
      durationArgs = if segmentCount <= 1
                     then ["-t", show layoutDuration]  -- Use total duration for single input
                     else []  -- Multi-segment handling would need concatenation
      
      filterComplex = if segmentCount == 0
                      then ["-filter_complex", "[0:v]scale=1920:1080[outv]", "-map", "[outv]"]
                      else if segmentCount == 1
                           then []  -- No filter complex needed for single input
                           else ["-filter_complex", generateFilterString segmentCount, "-map", "[outv]", "-map", "[outa]"]
  in [ ffmpegBinary config, "-y"
     , "-loglevel", if verboseLogging config then "info" else "warning"
     ] ++ inputArgs ++ filterComplex ++ durationArgs ++
     [ "-c:v", "libx264", "-preset", "medium"
     , "-crf", "23", "-pix_fmt", "yuv420p", outputPath'
     ]

-- | Get input arguments for a specific segment
getInputArgs :: MediaSources -> (Int, VideoSegment) -> [String]
getInputArgs sources (_, segment) = 
  case segmentType segment of
    VideoClip mediaRef -> 
      let Duration startOffset = startTime mediaRef
          Duration endOffset = endTime mediaRef
          clipDuration = endOffset - startOffset
          filePath = resolveVideoPath sources mediaRef
          seekArgs = if startOffset > 0 then ["-ss", show startOffset] else []
          durationArgs = ["-t", show clipDuration]
      in seekArgs ++ durationArgs ++ ["-i", filePath]
    PhotoClip mediaRef duration -> 
      let Duration photoDuration = duration
          filePath = resolvePhotoPath sources mediaRef
      in ["-loop", "1", "-t", show photoDuration, "-i", filePath]
    TitleCard _ duration -> 
      let Duration titleDuration = duration
      in ["-f", "lavfi", "-i", "color=black:size=1920x1080:duration=" ++ show titleDuration]
    TransitionSegment _ -> [] -- Transitions don't need separate inputs

-- | Resolve video file path from MediaSources
resolveVideoPath :: MediaSources -> MediaReference -> FilePath
resolveVideoPath sources mediaRef = videoSourceDir sources </> T.unpack (mediaId mediaRef)

-- | Resolve photo file path from MediaSources  
resolvePhotoPath :: MediaSources -> MediaReference -> FilePath
resolvePhotoPath sources mediaRef = photoSourceDir sources </> T.unpack (mediaId mediaRef)

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