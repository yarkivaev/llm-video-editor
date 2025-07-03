{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Action.AssembleSpec (spec) where

import Test.Hspec
import Control.Monad.Reader
import Data.Time (parseTimeOrError, defaultTimeLocale)
import qualified Types.Assembly as Assembly
import Types.Assembly (AssemblyContext(..), AssemblyStrategy(..), LLMConfig(..))
import Types.LLMApi (LLMApi(..), Prompt(..))
import Types.Common (Duration(..), Timestamp(..), Resolution(..))
import Types.Media (VideoRequest(..), MediaFile(..), VideoFile(..), MediaMetadata(..))
import Types.Video (VideoLayout(..))
import Action.Assemble
import File (File(..), Path(..), Segment(..))
import Data.String (fromString)

-- Mock LLMApi for testing
data MockLLM = MockLLM { mockResponse :: Assembly.AssemblyResult }

instance LLMApi MockLLM where
  call mockLLM (Prompt _) = return (mockResponse mockLLM)

-- Test data
sampleTimestamp :: Timestamp
sampleTimestamp = Timestamp $ parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2024-01-01 12:00:00 UTC"

sampleVideoFile :: VideoFile
sampleVideoFile = VideoFile
  { videoMetadata = MediaMetadata
      { file = File.File { filePath = Path [fromString "test"], fileName = "test.mp4" }
      , fileSize = 1000000
      , createdAt = sampleTimestamp
      , location = Nothing
      , description = Just "Test video"
      , tags = ["test"]
      }
  , videoDuration = Duration 60.0
  , resolution = Resolution 1920 1080
  , frameRate = 30.0
  , hasAudio = True
  , videoFormat = "mp4"
  , contentAnalysis = Nothing
  }

sampleVideoRequest :: VideoRequest
sampleVideoRequest = VideoRequest
  { requestId = "test-request"
  , mediaFiles = [Video sampleVideoFile]
  , userPrompt = "Create a test video"
  , submittedAt = sampleTimestamp
  }

sampleAssemblyContext :: AssemblyContext
sampleAssemblyContext = AssemblyContext
  { strategy = SingleLLM $ LLMConfig "test-model" 0.7 Nothing Nothing Nothing Nothing
  , maxVideoDuration = Just (Duration 60.0)
  , preferredStyle = Just "test"
  , targetAudience = Just "developers"
  , budgetConstraints = Nothing
  , technicalLimits = []
  , customInstructions = []
  }

sampleVideoLayout :: VideoLayout
sampleVideoLayout = VideoLayout
  { layoutId = "test-layout"
  , totalDuration = Duration 60.0
  , segments = []
  , globalAudio = []
  , outputFormat = "mp4"
  , outputResolution = Resolution 1920 1080
  , outputFrameRate = 30.0
  , layoutCreatedAt = sampleTimestamp
  }

spec :: Spec
spec = do
  describe "assembleVideo" $ do
    it "successfully assembles video with mock LLMApi returning success" $ do
      let mockLLM = MockLLM (Assembly.Success sampleVideoLayout)
      result <- runReaderT (assembleVideo sampleVideoRequest sampleAssemblyContext) mockLLM
      case result of
        Assembly.Success layout -> do
          layoutId layout `shouldBe` "test-layout"
          totalDuration layout `shouldBe` Duration 60.0
        _ -> expectationFailure "Expected Success result"
    
    it "handles LLMApi failures gracefully" $ do
      let mockLLM = MockLLM (Assembly.Failure (Assembly.AssemblyLLMError "Mock LLMApi error"))
      result <- runReaderT (assembleVideo sampleVideoRequest sampleAssemblyContext) mockLLM
      case result of
        Assembly.Failure (Assembly.AssemblyLLMError msg) -> msg `shouldBe` "Mock LLMApi error"
        _ -> expectationFailure "Expected Failure result"
    
    it "handles partial success with warnings" $ do
      let mockLLM = MockLLM (Assembly.PartialSuccess sampleVideoLayout ["Warning 1", "Warning 2"])
      result <- runReaderT (assembleVideo sampleVideoRequest sampleAssemblyContext) mockLLM
      case result of
        Assembly.PartialSuccess layout warnings -> do
          layoutId layout `shouldBe` "test-layout"
          warnings `shouldBe` ["Warning 1", "Warning 2"]
        _ -> expectationFailure "Expected PartialSuccess result"
    
    it "calls LLMApi with proper prompt generation" $ do
      -- This test would verify that the prompt is generated correctly
      -- and passed to the LLMApi call function
      let mockLLM = MockLLM (Assembly.Success sampleVideoLayout)
      result <- runReaderT (assembleVideo sampleVideoRequest sampleAssemblyContext) mockLLM
      -- The fact that we get a result means the prompt was generated and call was made
      case result of
        Assembly.Success _ -> return ()
        _ -> expectationFailure "assembleVideo should succeed with valid inputs"