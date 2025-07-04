{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.System
  ( VideoEditorInput (..)
  , VideoEditorOutput (..)
  , ProcessingStatus (..)
  , VideoEditorError (..)
  , VideoEditor (..)
  , videoEditor
  ) where

import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Types.Media (VideoRequest(..), MediaFile(..), VideoFile(..), MediaMetadata(..))
import Types.Video (VideoLayout)
import Types.Render (OutputFile(..), RenderContext, RenderResult(..), VideoRenderer(..))
import Types.Assembly (AssemblyContext, AssemblyResult(..), VideoAssembler(..))
import Types.VideoAnalysis (VideoAnalysis(..))
import File

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
  } deriving (Eq, Generic, FromJSON, ToJSON)

-- | Output from the video editor system
data VideoEditorOutput = VideoEditorOutput
  { outputFile :: OutputFile
  , warnings :: [Text]
  } deriving (Eq, Generic, FromJSON, ToJSON)

class Monad m => VideoEditor m where
    edit :: VideoEditorInput -> m VideoEditorOutput

-- | Instance for any monad that has VideoAnalysis, VideoAssembler, and VideoRenderer capabilities
-- Requires AssemblyContext and RenderContext to be provided via MonadReader or similar
instance (VideoAnalysis m, VideoAssembler m, VideoRenderer m, MonadReader (AssemblyContext, RenderContext) m) => VideoEditor m where
    edit input = do
        (assemblyCtx, renderCtx) <- ask
        result <- videoEditor assemblyCtx renderCtx input
        case result of
            Left err -> error $ "VideoEditor failed: " <> show err  -- Or use MonadError if available
            Right output -> return output

-- | Main video editor implementation that orchestrates the entire pipeline
videoEditor :: (VideoAnalysis m, VideoAssembler m, VideoRenderer m) 
           => AssemblyContext 
           -> RenderContext 
           -> VideoEditorInput 
           -> m (Either VideoEditorError VideoEditorOutput)
videoEditor assemblyCtx renderCtx (VideoEditorInput request) = do
  -- Step 1: Analyze all video files in the request to get content analysis
  analyzedRequest <- analyzeMediaFiles request
  
  case analyzedRequest of
    Left err -> return $ Left err
    Right enrichedRequest -> do
      -- Step 2: Use VideoAssembler to create video layout from enriched request
      assemblyResult <- assembleVideo enrichedRequest assemblyCtx
      
      case assemblyResult of
        Failure assemblyErr -> return $ Left $ LLMError $ T.pack ("Assembly failed: " <> show assemblyErr)
        Success layout -> renderVideoLayout layout renderCtx []
        PartialSuccess layout partialWarnings -> renderVideoLayout layout renderCtx partialWarnings

-- | Helper function to analyze media files and enrich VideoRequest
analyzeMediaFiles :: VideoAnalysis m => VideoRequest -> m (Either VideoEditorError VideoRequest)
analyzeMediaFiles request = do
  let files = mediaFiles request
  analyzedFiles <- mapM analyzeMediaFile files
  
  case sequence analyzedFiles of
    Left err -> return $ Left err
    Right enrichedFiles -> return $ Right $ request { mediaFiles = enrichedFiles }

-- | Analyze a single media file
analyzeMediaFile :: VideoAnalysis m => MediaFile -> m (Either VideoEditorError MediaFile)
analyzeMediaFile (Video videoFile) = do
  case contentAnalysis videoFile of
    Just _ -> return $ Right $ Video videoFile -- Already analyzed
    Nothing -> do
      let mediaFile = file . videoMetadata $ videoFile
      analysis <- analyze mediaFile
      let enrichedVideoFile = videoFile { contentAnalysis = Just analysis }
      return $ Right $ Video enrichedVideoFile
analyzeMediaFile photoFile = return $ Right photoFile -- Photos don't need analysis for now

-- | Helper function to render video layout and create output
renderVideoLayout :: VideoRenderer m 
                  => VideoLayout 
                  -> RenderContext 
                  -> [Text] 
                  -> m (Either VideoEditorError VideoEditorOutput)
renderVideoLayout layout renderCtx warningList = do
  renderResult <- renderVideo layout renderCtx
  
  case renderResult of
    RenderFailure renderErr -> return $ Left $ ProcessingError $ T.pack $ "Video rendering failed: " <> show renderErr
    RenderSuccess outputFilePath -> return $ Right $ VideoEditorOutput 
      { outputFile = OutputFile outputFilePath
      , warnings = warningList
      }