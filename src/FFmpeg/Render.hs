module FFmpeg.Render
  ( 
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, Reader)
import Types.Render
import FFmpeg.Config

-- instance (MonadIO m, MonadReader FFmpegConfig m) => VideoRenderer m where
--   renderVideo layout context = do
--     config <- FFmpegRenderer ask
--     liftIO $ do
--       -- Create temp directory
--       createDirectoryIfMissing True (tempDir config)
      
--       -- Validate inputs
--       validationResult <- validateInputs layout context
--       case validationResult of
--         Left err -> return $ RenderFailure err
--         Right _ -> do
--           -- Generate FFmpeg command
--           ffmpegCmd <- generateFFmpegCommand config layout context
--           putStrLn (unwords ffmpegCmd) --HOTFIX

--           -- Execute FFmpeg
--           result <- executeFFmpeg config ffmpegCmd
--           case result of
--             Left err -> return $ RenderFailure err
--             Right outputFile -> return $ RenderSuccess outputFile