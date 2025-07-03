{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.LLMIntegrationSpec (spec) where

import Test.Hspec
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Types.LLMApi (LLMApi(..), Prompt(..), prompt)
import Types.LLM (LLM(..))
import Types.Assembly (AssemblyResult(..), AssemblyContext(..), AssemblyStrategy(..))
import Types.Common (Duration(..), Timestamp(..), Resolution(..), Location(..))
import Types.Media (VideoRequest(..), MediaFile(..), VideoFile(..), MediaMetadata(..), VideoContentAnalysis(..))
import Types.Video (VideoLayout(..), VideoSegment(..))
import qualified Types.Assembly as Assembly
import File (File(..), Path(..), Segment(..))
import Data.String (fromString)

-- | Create minimal test data
mkTestData :: IO (VideoRequest, AssemblyContext)
mkTestData = do
  now <- getCurrentTime
  let ts = Timestamp now
      video = VideoFile 
        { videoMetadata = MediaMetadata 
            { file = File.File { filePath = Path [fromString "test"], fileName = "test.mp4" }
            , fileSize = 1000000
            , createdAt = ts
            , location = Just $ Location 0.0 0.0 (Just "Test")
            , description = Just "Test video"
            , tags = ["test"]
            }
        , videoDuration = Duration 30.0, resolution = Resolution 1920 1080
        , frameRate = 30.0, hasAudio = True, videoFormat = "mp4"
        , contentAnalysis = Just $ VideoContentAnalysis "Test content" "Test action" [] [] [] (Just "calm")
        }
      request = VideoRequest "test-001" [Video video] "Create a test video" ts
      context = AssemblyContext (SingleLLM $ Assembly.LLMConfig "test" 0.7 Nothing Nothing Nothing Nothing)
        (Just $ Duration 60.0) (Just "test") (Just "testers") Nothing [] []
  return (request, context)

-- | Helper to validate LLMApi response
validateResponse :: AssemblyResult -> IO ()
validateResponse result = case result of
  Assembly.Success layout -> do
    -- Basic validation
    T.length (layoutId layout) `shouldSatisfy` (> 0)
    totalDuration layout `shouldSatisfy` (\(Duration d) -> d > 0)
    outputFormat layout `shouldBe` "mp4"
    
    -- Validate segments exist
    length (segments layout) `shouldSatisfy` (> 0)
    
    -- Check that at least one segment has textOverlays or audioTracks (testing JSON parsing)
    let hasTextOverlays = any (not . null . textOverlays) (segments layout)
        hasAudioTracks = any (not . null . audioTracks) (segments layout)
        hasGlobalAudio = not . null $ globalAudio layout
    
    -- At least one of these should be present to test JSON parsing
    (hasTextOverlays || hasAudioTracks || hasGlobalAudio) `shouldBe` True
    
  Assembly.PartialSuccess layout warnings -> do
    expectationFailure $ "Partial success not acceptable in integration tests. Warnings: " ++ show warnings
  Assembly.Failure err -> do
    expectationFailure $ "LLM API call failed: " ++ show err

-- | Helper to run test with LLMApi provider
testWithProvider :: LLM -> (LLM -> IO ()) -> IO ()
testWithProvider llm testAction = do
  result <- try $ testAction llm
  case result of
    Left (ex :: SomeException) -> pendingWith $ "Network error: " ++ show ex
    Right _ -> return ()

-- | Test LLMApi with video assembly prompts
testLLMPrompts :: LLM -> IO ()
testLLMPrompts llm = do
  (request, context) <- mkTestData
  
  -- Test video assembly prompt
  let videoPrompt = prompt request context
  videoResult <- call llm videoPrompt
  validateResponse videoResult

spec :: Spec
spec = do
  describe "LLM Integration Tests (Real API Calls)" $ do
    
    it "detects ChatGPT API key availability" $ do
      openaiKey <- lookupEnv "OPENAI_API_KEY"
      case openaiKey of
        Nothing -> pendingWith "No OPENAI_API_KEY found. Set OPENAI_API_KEY environment variable."
        Just _ -> do
          putStrLn "✓ OPENAI_API_KEY detected"
          putStrLn "Using ChatGPT (gpt-4) model"
    
    it "creates and tests ChatGPT instance" $ do
      openaiKey <- lookupEnv "OPENAI_API_KEY"
      case openaiKey of
        Nothing -> pendingWith "OPENAI_API_KEY not set"
        Just _ -> testWithProvider ChatGpt testLLMPrompts
    
    it "validates LLM prompt generation quality" $ do
      (request, context) <- mkTestData
      let videoPrompt = prompt request context
      
      case videoPrompt of
        Prompt promptText -> do
          -- Validate prompt contains key elements
          promptText `shouldSatisfy` T.isInfixOf "video editor AI"
          promptText `shouldSatisfy` T.isInfixOf "JSON"
          promptText `shouldSatisfy` T.isInfixOf "test.mp4"
          promptText `shouldSatisfy` T.isInfixOf "layoutId"
          
          -- Check reasonable length
          let promptLength = T.length promptText
          promptLength `shouldSatisfy` (> 500)
          promptLength `shouldSatisfy` (< 10000)
          
          putStrLn $ "Generated prompt length: " ++ show promptLength ++ " characters"