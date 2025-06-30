{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Action.Assemble
  ( assembleVideo
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Types.Media (VideoRequest)
import Types.LLM
import Types.Assembly


instance (LLM a, MonadIO m, MonadReader a m) => VideoAssembler m where
  assembleVideo request context = do
    llmInstance <- ask
    let promptObj = prompt request context
    liftIO $ call llmInstance promptObj

