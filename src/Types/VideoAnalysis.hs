module Types.VideoAnalysis 
    ( VideoAnalysis
    ) where

import Types.Media (VideoContentAnalysis)

import File

class Monad m => VideoAnalysis m where
    analyze :: File -> m VideoContentAnalysis
