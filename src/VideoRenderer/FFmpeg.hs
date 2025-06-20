{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VideoRenderer.FFmpeg
  ( FFmpegRenderer (..)
  , FFmpegConfig (..)
  , defaultFFmpegConfig
  , runFFmpegRenderer
  , generateFFmpegCommand
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeExtension, dropExtension)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

import Types.Common (Duration(..), Resolution(..))
import Types.Video
import VideoRenderer

-- | Configuration for FFmpeg renderer
data FFmpegConfig = FFmpegConfig
  { ffmpegBinary    :: FilePath    -- ^ Path to ffmpeg binary
  , ffprobeBinary   :: FilePath    -- ^ Path to ffprobe binary  
  , tempDir         :: FilePath    -- ^ Directory for temporary files
  , maxConcurrency  :: Int         -- ^ Maximum concurrent processes
  , timeoutSeconds  :: Int         -- ^ Process timeout in seconds
  , verboseLogging  :: Bool        -- ^ Enable verbose logging
  } deriving (Show, Eq, Generic)

-- | Default FFmpeg configuration
defaultFFmpegConfig :: FFmpegConfig
defaultFFmpegConfig = FFmpegConfig
  { ffmpegBinary = "ffmpeg"
  , ffprobeBinary = "ffprobe"
  , tempDir = "/tmp/llm-video-editor"
  , maxConcurrency = 4
  , timeoutSeconds = 3600  -- 1 hour
  , verboseLogging = False
  }

-- | FFmpeg renderer monad
newtype FFmpegRenderer a = FFmpegRenderer (ReaderT FFmpegConfig IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run FFmpeg renderer with configuration
runFFmpegRenderer :: FFmpegConfig -> FFmpegRenderer a -> IO a
runFFmpegRenderer config (FFmpegRenderer action) = runReaderT action config

instance VideoRenderer FFmpegRenderer where
  renderVideo layout context = do
    config <- FFmpegRenderer ask
    liftIO $ do
      -- Create temp directory
      createDirectoryIfMissing True (tempDir config)
      
      -- Validate inputs
      validationResult <- validateInputs layout context
      case validationResult of
        Left err -> return $ RenderFailure err
        Right _ -> do
          -- Generate FFmpeg command
          ffmpegCmd <- generateFFmpegCommand config layout context
          putStrLn (unwords ffmpegCmd) --HOTFIX

          -- Execute FFmpeg
          result <- executeFFmpeg config ffmpegCmd
          case result of
            Left err -> return $ RenderFailure err
            Right outputFile -> return $ RenderSuccess outputFile

  validateRender layout context = do
    liftIO $ validateInputs layout context

  estimateRenderTime layout _context = do
    let Duration totalDur = totalDuration layout
    -- Rough estimate: 1 second of video takes 2 seconds to render
    return $ max 10.0 (totalDur * 2.0)

  getRendererCapabilities = return
    [ "ffmpeg-based"
    , "supports_mp4"
    , "supports_mov" 
    , "supports_transitions"
    , "supports_audio_mixing"
    , "supports_text_overlays"
    ]

-- | Validate input files and context
validateInputs :: VideoLayout -> RenderContext -> IO (Either RenderError ())
validateInputs layout context = do
  let MediaSources{videoSourceDir=videoDir, photoSourceDir=photoDir, audioSourceDir=audioDir} = mediaSources context
      OutputPath outputFile = outputPath context
  
  -- Check if source directories exist
  videoDirExists <- doesDirectoryExist videoDir
  photoDirExists <- doesDirectoryExist photoDir  
  audioDirExists <- doesDirectoryExist audioDir
  
  -- Check if output directory is writable
  let outputDir = takeDirectory outputFile
  createDirectoryIfMissing True outputDir
  
  -- Validate media references in segments
  mediaValidation <- validateMediaReferences (segments layout) context
  
  case mediaValidation of
    Left err -> return $ Left err
    Right _ -> 
      if videoDirExists && photoDirExists && audioDirExists
        then return $ Right ()
        else return $ Left $ RenderFileNotFound "Media source directory not found"

-- | Validate media references in video segments
validateMediaReferences :: [VideoSegment] -> RenderContext -> IO (Either RenderError ())
validateMediaReferences segments context = do
  let MediaSources{videoSourceDir=videoDir, photoSourceDir=photoDir, audioSourceDir=audioDir} = mediaSources context
  -- For now, just return success - in a real implementation, 
  -- we'd check each media reference exists
  return $ Right ()

-- | Generate FFmpeg command for the video layout
generateFFmpegCommand :: FFmpegConfig -> VideoLayout -> RenderContext -> IO [String]
generateFFmpegCommand config layout context = do
  let OutputPath outputFile = outputPath context
      opts = renderOptions context
      Resolution width height = outputResolution layout
      MediaSources{videoSourceDir=videoDir, photoSourceDir=photoDir, audioSourceDir=audioDir} = mediaSources context
      
  -- Generate input commands from segments
  inputCmds <- generateInputCommands (segments layout) context
      
  -- If no inputs from segments, create black background
  let backgroundCmd = if null inputCmds
        then [ "-f", "lavfi"
             , "-i", printf "color=black:size=%dx%d:duration=%.2f:rate=%.2f" 
                       (width :: Int) (height :: Int) (getDuration $ totalDuration layout :: Double) (outputFrameRate layout :: Double) :: String
             ]
        else []
        
  -- Generate filter complex for combining segments
  filterCmd <- generateFilterComplex (segments layout) layout
  
  -- Basic FFmpeg command structure  
  let baseCmd = ["-y"]  -- overwrite output file
      
      -- Add codec options
      codecCmd = case codec opts of
        Just c -> ["-c:v", T.unpack c]
        Nothing -> ["-c:v", "libx264"]
        
      -- Add quality options
      qualityCmd = case bitrate opts of
        Just b -> ["-b:v", T.unpack b]
        Nothing -> ["-crf", "23"]  -- default quality
        
      -- Add audio codec
      audioCmd = case audioCodec opts of
        Just ac -> ["-c:a", T.unpack ac]
        Nothing -> ["-c:a", "aac"]
        
      -- Output file
      outputCmd = [outputFile]
  
  return $ baseCmd ++ inputCmds ++ backgroundCmd ++ filterCmd ++ codecCmd ++ qualityCmd ++ audioCmd ++ outputCmd
  where
    getDuration (Duration d) = d

-- | Execute FFmpeg command
executeFFmpeg :: FFmpegConfig -> [String] -> IO (Either RenderError FilePath)
executeFFmpeg config args = do
  let cmd = ffmpegBinary config
      outputFile = last args
      
  when (verboseLogging config) $ do
    putStrLn $ "Executing: " ++ cmd ++ " " ++ unwords args
    
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args ""
  
  case exitCode of
    ExitSuccess -> do
      when (verboseLogging config) $ 
        putStrLn $ "FFmpeg completed successfully"
      return $ Right outputFile
    ExitFailure code -> do
      let errorMsg = "FFmpeg failed with exit code " ++ show code ++ 
                    ": " ++ stderr
      when (verboseLogging config) $ 
        putStrLn errorMsg
      return $ Left $ RenderProcessError $ T.pack errorMsg

-- | Generate input commands for media files referenced in segments
generateInputCommands :: [VideoSegment] -> RenderContext -> IO [String]
generateInputCommands segments context = do
  let MediaSources{videoSourceDir=videoDir, photoSourceDir=photoDir, audioSourceDir=audioDir} = mediaSources context
  
  -- Extract unique media references from segments
  let mediaRefs = concatMap extractMediaReferences segments
      uniqueRefs = removeDuplicates mediaRefs
  
  -- Generate input commands for each unique media file
  inputs <- mapM (generateSingleInput videoDir photoDir) uniqueRefs
  return $ concat inputs
  where
    extractMediaReferences :: VideoSegment -> [MediaReference]
    extractMediaReferences seg = case segmentType seg of
      VideoClip ref -> [ref]
      PhotoClip ref _ -> [ref]
      _ -> []
    
    removeDuplicates :: [MediaReference] -> [MediaReference]
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
    
    generateSingleInput :: FilePath -> FilePath -> MediaReference -> IO [String]
    generateSingleInput videoDir photoDir (MediaReference mediaId startTime endTime playbackSpeed) = do
      let mediaFile = T.unpack mediaId
          -- Try video directory first, then photo directory
          videoPath = videoDir </> mediaFile
          photoPath = photoDir </> mediaFile
      
      videoExists <- doesFileExist videoPath
      photoExists <- doesFileExist photoPath
      
      let filePath = if videoExists then videoPath 
                    else if photoExists then photoPath
                    else videoPath  -- fallback, will error later
          isPhoto = not videoExists && photoExists
      
      -- Generate input with seeking if start time > 0
      let Duration startSecs = startTime
          Duration endSecs = endTime
          duration = endSecs - startSecs
          
          seekCmd = if startSecs > 0 
                   then ["-ss", printf "%.2f" (startSecs :: Double) :: String]
                   else []
          
          durationCmd = if duration > 0
                       then ["-t", printf "%.2f" (duration :: Double) :: String]
                       else []
                       
          -- For photos, we need to loop them (before -i)
          loopCmd = if isPhoto
                   then ["-loop", "1"]
                   else []
      
      return $ seekCmd ++ durationCmd ++ loopCmd ++ ["-i", filePath]

-- | Generate filter complex for combining and sequencing segments  
generateFilterComplex :: [VideoSegment] -> VideoLayout -> IO [String]
generateFilterComplex segments layout = do
  if null segments
    then return []
    else do
      let Resolution width height = outputResolution layout
          fps = outputFrameRate layout
          numSegments = length segments
          
      if numSegments == 1
        then do
          -- Single segment - just scale and process directly
          let filterGraph = printf "[0:v]scale=%d:%d:force_original_aspect_ratio=decrease,pad=%d:%d:(ow-iw)/2:(oh-ih)/2,setsar=1:1[outv];[0:a]acopy[outa]" 
                              (width :: Int) (height :: Int) (width :: Int) (height :: Int) :: String
          return ["-filter_complex", filterGraph, "-map", "[outv]", "-map", "[outa]"]
        else do
          -- Multiple segments - need to concatenate
          let segmentFilters = zipWith (createSegmentFilter width height fps) [0..] segments
              segmentLabels = concatMap (\i -> printf "[%dv][%da]" (i :: Int) (i :: Int) :: String) [0..numSegments-1]
              concatFilter = printf "%sconcat=n=%d:v=1:a=1[outv][outa]"
                              (segmentLabels :: String)
                              (numSegments :: Int) :: String
              filterGraph = intercalateStr ";" segmentFilters ++ ";" ++ concatFilter
          return ["-filter_complex", filterGraph, "-map", "[outv]", "-map", "[outa]"]
  where    
    createSegmentFilter :: Int -> Int -> Double -> Int -> VideoSegment -> String  
    createSegmentFilter w h fps inputIndex seg =
      let Duration startSecs = segmentStart seg
          Duration endSecs = segmentEnd seg
          duration = endSecs - startSecs
      in case segmentType seg of
           VideoClip _ -> 
             printf "[%d:v]scale=%d:%d:force_original_aspect_ratio=decrease,pad=%d:%d:(ow-iw)/2:(oh-ih)/2,setsar=1:1,setpts=PTS-STARTPTS[%dv];[%d:a]asetpts=PTS-STARTPTS[%da]"
               (inputIndex :: Int) (w :: Int) (h :: Int) (w :: Int) (h :: Int) (inputIndex :: Int) (inputIndex :: Int) (inputIndex :: Int) :: String
           PhotoClip _ _ ->
             printf "[%d:v]scale=%d:%d:force_original_aspect_ratio=decrease,pad=%d:%d:(ow-iw)/2:(oh-ih)/2,setsar=1:1,setpts=PTS-STARTPTS[%dv];[%d:a]asetpts=PTS-STARTPTS[%da]"  
               (inputIndex :: Int) (w :: Int) (h :: Int) (w :: Int) (h :: Int) (inputIndex :: Int) (inputIndex :: Int) (inputIndex :: Int) :: String
           _ -> 
             printf "color=black:size=%dx%d:duration=%.2f:rate=%.2f[%dv];aevalsrc=0:d=%.2f[%da]"
               (w :: Int) (h :: Int) (duration :: Double) (fps :: Double) (inputIndex :: Int) (duration :: Double) (inputIndex :: Int) :: String

-- Helper function for when
when :: Applicative f => Bool -> f () -> f ()
when True action = action
when False _ = pure ()

-- Helper function for string intercalation
intercalateStr :: String -> [String] -> String
intercalateStr _ [] = ""
intercalateStr _ [x] = x
intercalateStr sep (x:xs) = x ++ sep ++ intercalateStr sep xs