module Main where

import qualified Lib
import qualified Types
import qualified Types.Common as Common
import qualified Types.Media as Media  
import qualified Types.Video as Video
import qualified Types.Assembly as Assembly
import qualified Types.System as System
import qualified VideoAssembler.LLM as LLM
import qualified VideoExporter
import qualified VideoRenderer
import qualified VideoRenderer.FFmpeg as FFmpeg

import Example

main :: IO ()
main = do
  result <- renderResult
  putStrLn $ show result