{-# LANGUAGE OverloadedStrings #-}

module VideoRendererSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose, hPutStr)

import Types.Video
import VideoRenderer

spec :: Spec
spec = describe "VideoRenderer" $ do
  
  describe "MediaSources" $ do
    it "has correct field accessors" $ do
      let sources = MediaSources "/video" "/photo" "/audio"
      videoSourceDir sources `shouldBe` "/video"
      photoSourceDir sources `shouldBe` "/photo" 
      audioSourceDir sources `shouldBe` "/audio"
    
    it "can be encoded to and decoded from JSON" $ do
      let sources = MediaSources "/v" "/p" "/a"
          encoded = encode sources
          decoded = decode encoded
      decoded `shouldBe` Just sources
  
  describe "OutputPath" $ do  
    it "wraps filepath correctly" $ do
      let OutputPath path = OutputPath "/output/test.mp4"
      path `shouldBe` "/output/test.mp4"
    
    it "can be encoded to and decoded from JSON" $ do
      let outputPath = OutputPath "/test.mp4"
          encoded = encode outputPath
          decoded = decode encoded
      decoded `shouldBe` Just outputPath
  
  describe "RenderOptions" $ do
    it "has correct default values" $ do
      let opts = defaultRenderOptions
      quality opts `shouldBe` Nothing
      codec opts `shouldBe` Nothing
      bitrate opts `shouldBe` Nothing
      audioCodec opts `shouldBe` Nothing
      audioBitrate opts `shouldBe` Nothing
      overwrite opts `shouldBe` False
    
    it "can be encoded to and decoded from JSON" $ do
      let opts = defaultRenderOptions { quality = Just "high", overwrite = True }
          encoded = encode opts
          decoded = decode encoded
      decoded `shouldBe` Just opts
  
  describe "RenderContext" $ do
    it "combines all render configuration" $ do
      let sources = MediaSources "/v" "/p" "/a"
          output = OutputPath "/out.mp4"
          opts = defaultRenderOptions
          context = RenderContext sources output opts
      mediaSources context `shouldBe` sources
      outputPath context `shouldBe` output
      renderOptions context `shouldBe` opts
  
  describe "RenderError" $ do
    it "represents different error types" $ do
      let fileNotFound = RenderFileNotFound "/missing.mp4"
          invalidInput = RenderInvalidInput "Bad format"
          processError = RenderProcessError "FFmpeg failed"
      
      show fileNotFound `shouldContain` "RenderFileNotFound"
      show invalidInput `shouldContain` "RenderInvalidInput"  
      show processError `shouldContain` "RenderProcessError"
  
  describe "RenderResult" $ do
    it "represents success and failure cases" $ do
      let success = RenderSuccess "/output.mp4"
          failure = RenderFailure (RenderFileNotFound "/missing")
      
      case success of
        RenderSuccess path -> path `shouldBe` "/output.mp4"
        _ -> expectationFailure "Expected RenderSuccess"
      
      case failure of
        RenderFailure (RenderFileNotFound path) -> path `shouldBe` "/missing"
        _ -> expectationFailure "Expected RenderFailure"
  
  describe "parseVideoLayout" $ do
    it "parses valid JSON file" $ do
      let validJson = "{ \
        \\"layoutId\": \"test\", \
        \\"totalDuration\": 30, \
        \\"segments\": [], \
        \\"globalAudio\": [], \
        \\"outputFormat\": \"mp4\", \
        \\"outputResolution\": {\"width\": 1920, \"height\": 1080}, \
        \\"outputFrameRate\": 30, \
        \\"layoutCreatedAt\": \"2024-01-01T00:00:00Z\" \
        \}"
      
      withSystemTempFile "test.json" $ \filePath handle -> do
        hPutStr handle validJson
        hClose handle
        result <- parseVideoLayout filePath
        case result of
          Right layout -> do
            layoutId layout `shouldBe` "test"
            outputFormat layout `shouldBe` "mp4"
          Left err -> expectationFailure $ "Parse should succeed: " ++ err
    
    it "fails on invalid JSON file" $ do
      let invalidJson = "{ invalid json }"
      
      withSystemTempFile "invalid.json" $ \filePath handle -> do
        hPutStr handle invalidJson
        hClose handle
        result <- parseVideoLayout filePath
        case result of
          Left _err -> return () -- Expected failure
          Right _layout -> expectationFailure "Parse should fail on invalid JSON"
    
    it "fails on non-existent file" $ do
      result <- parseVideoLayout "/nonexistent/file.json"
      case result of
        Left _err -> return () -- Expected failure
        Right _layout -> expectationFailure "Parse should fail on missing file"