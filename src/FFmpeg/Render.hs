{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module FFmpeg.Render
  ( 
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Types.Render
import Types.Transcoder
import FFmpeg.Config (FFmpegConfig(..))
import FFmpeg.Transcoder ()
import File
import FileSystem

instance (Monad m, MonadIO m, MonadReader FFmpegConfig m, MonadFileShow m) => VideoRenderer m where
  renderVideo layout context = do
    let request = TranscodeRequest
          { transcodeLayout = layout
          , transcodeMediaSources = mediaSources context
          , transcodeOutputPath = outputPath context
          }
    
    result <- transcode request
    case result of
      TranscodeSuccess command -> do
        -- Execute the command
        execResult <- liftIO $ executeTranscodeCommand command
        case execResult of
          Left err -> return $ RenderFailure err
          Right outputFile -> return $ RenderSuccess outputFile
      TranscodeFailure transcodeError -> return $ RenderFailure (convertTranscodeError transcodeError)

-- | Execute a transcode command
executeTranscodeCommand :: TranscodeCommand -> IO (Either RenderError File)
executeTranscodeCommand cmd = do
  let binaryPath = show (commandBinary cmd)
  result <- try $ readProcessWithExitCode binaryPath (commandArgs cmd) ""
  case result of
    Left (ex :: SomeException) -> 
      return $ Left $ RenderProcessError $ T.pack $ show ex
    Right (ExitSuccess, _, _) ->
      return $ Right $ commandOutputPath cmd
    Right (ExitFailure code, stdout, stderr) ->
      return $ Left $ RenderProcessError $ T.pack $ 
        "Command failed with exit code " ++ show code ++ 
        "\nStdout: " ++ stdout ++ 
        "\nStderr: " ++ stderr

-- | Convert TranscodeError to RenderError
convertTranscodeError :: TranscodeError -> RenderError
convertTranscodeError (TranscodeInputError msg) = RenderInvalidInput msg
convertTranscodeError (TranscodeProcessError msg) = RenderProcessError msg
convertTranscodeError (TranscodeOutputError msg) = RenderOutputError msg
convertTranscodeError (TranscodeFileNotFound path) = RenderFileNotFound path
convertTranscodeError TranscodeTimeout = RenderTimeoutError