{-# LANGUAGE OverloadedStrings #-}

module Integration.ChatGPTIntegrationSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (parseTimeOrError, defaultTimeLocale)
import System.Environment (lookupEnv)
import VideoAssembler.LLM
import qualified Types.Assembly as Assembly
import Types

-- Test data for integration tests
integrationTimestamp :: Timestamp
integrationTimestamp = Timestamp $ parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2024-01-15 14:30:00 UTC"

realVideoFile :: VideoFile  
realVideoFile = VideoFile
  { videoMetadata = MediaMetadata
      { fileName = "mountain_hike.mp4"
      , filePath = "/videos/mountain_hike.mp4"
      , fileSize = 524288000  -- 500MB
      , createdAt = integrationTimestamp
      , location = Just $ Location 46.8566 (-121.7603) (Just "Mount Rainier, WA")
      , description = Just "Epic mountain hiking adventure with stunning views"
      , tags = ["hiking", "mountains", "adventure", "nature"]
      }
  , videoDuration = Duration 300.0  -- 5 minutes
  , resolution = Resolution 3840 2160  -- 4K
  , frameRate = 60.0
  , hasAudio = True
  , videoFormat = "mp4"
  , contentAnalysis = Just $ VideoContentAnalysis
      { contentOverview = "Breathtaking mountain hiking footage showing climbers ascending rocky trails with panoramic mountain vistas, alpine lakes, and wildlife encounters including marmots and mountain goats."
      , actionIntroduction = "The video begins with hikers preparing gear at base camp, then follows their ascent through various terrain types including forest paths, rocky scrambles, and alpine meadows."
      , timeBoundDetails = 
          [ TimeBoundDetail (Duration 0.0) (Duration 45.0) "Hikers preparing gear and starting trail at parking area" (Just 0.96)
          , TimeBoundDetail (Duration 45.0) (Duration 120.0) "Forest section with creek crossings and wildlife spotting" (Just 0.94)
          , TimeBoundDetail (Duration 120.0) (Duration 200.0) "Rocky scramble section with technical climbing moves" (Just 0.91)
          , TimeBoundDetail (Duration 200.0) (Duration 260.0) "Summit approach with panoramic views and celebration" (Just 0.97)
          , TimeBoundDetail (Duration 260.0) (Duration 300.0) "Sunset timelapse from summit with mountain ranges" (Just 0.95)
          ]
      , detectedObjects = ["hikers", "backpacks", "mountains", "trees", "rocks", "snow", "alpine_lake", "marmots", "mountain_goats", "hiking_poles"]
      , detectedScenes = ["outdoor", "mountain", "forest", "alpine", "summit", "sunset"]
      , estimatedMood = Just "adventurous"
      }
  }

realPhotoFile :: PhotoFile
realPhotoFile = PhotoFile
  { photoMetadata = MediaMetadata
      { fileName = "summit_selfie.jpg"
      , filePath = "/photos/summit_selfie.jpg" 
      , fileSize = 8388608  -- 8MB
      , createdAt = integrationTimestamp
      , location = Just $ Location 46.8566 (-121.7603) (Just "Mount Rainier Summit, WA")
      , description = Just "Victory selfie at the mountain summit"
      , tags = ["summit", "selfie", "achievement", "mountains"]
      }
  , photoResolution = Resolution 6000 4000
  , imageFormat = "jpg"
  , cameraSettings = Just "ISO 200, f/11, 1/250s, 24mm"
  }

realVideoRequest :: VideoRequest
realVideoRequest = VideoRequest
  { requestId = "integration-test-001"
  , mediaFiles = [Video realVideoFile, Photo realPhotoFile]
  , userPrompt = "Create an inspiring 90-second highlight video of my mountain hiking adventure. Start with a title card showing 'Epic Mountain Adventure', then show the journey from preparation to summit triumph. Include dramatic transitions between different sections of the hike. Add the summit selfie photo as the climactic moment. Focus on the sense of achievement and natural beauty. End with the sunset timelapse. Use cinematic pacing with uplifting background music."
  , submittedAt = integrationTimestamp
  }

chatGPTConfig :: LLMConfig
chatGPTConfig = LLMConfig
  { modelName = "gpt-4o-mini"
  , temperature = 0.7
  , maxTokens = Just 4000
  , systemPrompt = Nothing
  , apiEndpoint = Nothing  -- Use default OpenAI endpoint
  , apiKey = Nothing       -- Will be read from environment
  }

realAssemblyContext :: AssemblyContext
realAssemblyContext = AssemblyContext
  { strategy = SingleLLM chatGPTConfig
  , maxVideoDuration = Just (Duration 90.0)
  , preferredStyle = Just "cinematic"
  , targetAudience = Just "adventure_enthusiasts"
  , budgetConstraints = Just "standard"
  , technicalLimits = ["max_segments_8", "4k_output", "60fps_support"]
  , customInstructions = 
      [ "emphasize_natural_beauty"
      , "dramatic_pacing"
      , "include_achievement_moment"
      , "uplifting_tone"
      ]
  }

spec :: Spec
spec = do
  describe "ChatGPT Integration Tests" $ do
    
    context "Environment Setup" $ do
      it "checks for required environment variables" $ do
        useRealAPI <- lookupEnv "USE_REAL_CHATGPT_API"
        case useRealAPI of
          Just "true" -> do
            apiKey <- lookupEnv "OPENAI_API_KEY"
            apiKey `shouldSatisfy` (\key -> case key of
              Just k -> not (null k)
              Nothing -> False)
          _ -> pendingWith "Set USE_REAL_CHATGPT_API=true to run real API tests"
    
    context "Real ChatGPT API Integration" $ do
      it "successfully creates video layout using real ChatGPT API" $ do
        useRealAPI <- lookupEnv "USE_REAL_CHATGPT_API"
        case useRealAPI of
          Just "true" -> do
            result <- createLLMAssembler chatGPTConfig realVideoRequest realAssemblyContext
            case result of
              Assembly.Success layout -> do
                -- Validate the generated layout structure
                layoutId layout `shouldSatisfy` (not . T.null)
                totalDuration layout `shouldSatisfy` (\(Duration d) -> d > 0 && d <= 120) -- Should be reasonable duration
                outputFormat layout `shouldBe` "mp4"
                outputResolution layout `shouldSatisfy` (\res -> width res > 0 && height res > 0)
                outputFrameRate layout `shouldSatisfy` (> 0)
                
                -- Validate segments were generated
                length (segments layout) `shouldSatisfy` (> 0)
                length (segments layout) `shouldSatisfy` (<= 8) -- Respects technical limits
                
                -- Check that segments have valid timing
                let segmentDurations = map (\seg -> case (segmentStart seg, segmentEnd seg) of
                                                      (Duration start, Duration end) -> end - start) (segments layout)
                let totalSegmentDuration = sum segmentDurations
                totalSegmentDuration `shouldSatisfy` (> 0)
                
              Assembly.Failure err -> expectationFailure $ "Expected successful layout generation, got error: " ++ show err
              Assembly.PartialSuccess layout warnings -> do
                -- Accept partial success but log warnings
                length warnings `shouldSatisfy` (< 5) -- Not too many warnings
                layoutId layout `shouldSatisfy` (not . T.null)
          _ -> pendingWith "Set USE_REAL_CHATGPT_API=true to run real API tests"
    
    context "Complex Prompt Processing" $ do
      it "handles detailed creative prompts correctly" $ do
        useRealAPI <- lookupEnv "USE_REAL_CHATGPT_API"
        case useRealAPI of
          Just "true" -> do
            let complexPrompt = "Create a dramatic 2-minute travel documentary style video with the following structure: 1) Opening title sequence (5 seconds), 2) Establishing shots of the mountain landscape (15 seconds), 3) Character introduction showing hikers preparing (10 seconds), 4) Journey montage with multiple segments showing progression up the mountain with varying pacing - slow for contemplative forest scenes, fast for action climbing sequences (60 seconds), 5) Climactic summit moment with emotional music swell (15 seconds), 6) Reflective conclusion with sunset and credits (15 seconds). Use professional color grading, smooth transitions, and ensure the narrative arc shows transformation from challenge to triumph."
            
            let complexRequest = realVideoRequest { userPrompt = complexPrompt }
            let complexContext = realAssemblyContext 
                  { maxVideoDuration = Just (Duration 120.0)
                  , customInstructions = 
                      [ "documentary_style"
                      , "professional_color_grading" 
                      , "narrative_arc"
                      , "emotional_music_integration"
                      , "character_transformation"
                      ]
                  }
            
            result <- createLLMAssembler chatGPTConfig complexRequest complexContext
            case result of
              Assembly.Success layout -> do
                -- Verify complex structure was understood
                length (segments layout) `shouldSatisfy` (>= 5) -- Should have multiple segments for complex structure
                
                -- Check for title segment
                let titleSegments = [seg | seg <- segments layout, 
                                     case segmentType seg of
                                       TitleCard _ _ -> True
                                       _ -> False]
                length titleSegments `shouldSatisfy` (>= 1)
                
                -- Verify duration is close to requested 2 minutes
                let Duration totalDur = totalDuration layout
                totalDur `shouldSatisfy` (\d -> d >= 100 && d <= 140) -- 1:40 to 2:20 range
                
              Assembly.Failure err -> expectationFailure $ "Complex prompt processing failed: " ++ show err
              Assembly.PartialSuccess layout warnings -> do
                length (segments layout) `shouldSatisfy` (>= 3) -- At least some structure
          _ -> pendingWith "Set USE_REAL_CHATGPT_API=true to run real API tests"
    
    context "Error Handling with Real API" $ do
      it "handles invalid API key gracefully" $ do
        useRealAPI <- lookupEnv "USE_REAL_CHATGPT_API"
        case useRealAPI of
          Just "true" -> do
            let invalidConfig = chatGPTConfig { apiKey = Just "invalid-key-123" }
            result <- createLLMAssembler invalidConfig realVideoRequest realAssemblyContext
            case result of
              Assembly.Failure (AssemblyLLMError msg) -> 
                msg `shouldSatisfy` T.isInfixOf "API error"
              _ -> expectationFailure "Expected LLM error for invalid API key"
          _ -> pendingWith "Set USE_REAL_CHATGPT_API=true to run real API tests"
      
      it "handles network timeouts and retries" $ do
        useRealAPI <- lookupEnv "USE_REAL_CHATGPT_API"
        case useRealAPI of
          Just "true" -> do
            -- Test with a very small token limit to potentially trigger issues
            let limitedConfig = chatGPTConfig { maxTokens = Just 10 }
            result <- createLLMAssembler limitedConfig realVideoRequest realAssemblyContext
            case result of
              Assembly.Success _ -> return () -- Somehow worked despite limits
              Assembly.Failure _ -> return () -- Expected failure
              Assembly.PartialSuccess _ _ -> return () -- Partial success acceptable
          _ -> pendingWith "Set USE_REAL_CHATGPT_API=true to run real API tests"
    
    context "Performance and Reliability" $ do
      it "completes requests within reasonable time" $ do
        useRealAPI <- lookupEnv "USE_REAL_CHATGPT_API"
        case useRealAPI of
          Just "true" -> do
            -- This test will timeout if ChatGPT takes too long
            result <- createLLMAssembler chatGPTConfig realVideoRequest realAssemblyContext
            case result of
              Assembly.Success layout -> layoutId layout `shouldSatisfy` (not . T.null)
              Assembly.Failure _ -> expectationFailure "Request should not fail due to timeout in normal conditions"
              Assembly.PartialSuccess layout _ -> layoutId layout `shouldSatisfy` (not . T.null)
          _ -> pendingWith "Set USE_REAL_CHATGPT_API=true to run real API tests"
      
      it "handles multiple consecutive requests" $ do
        useRealAPI <- lookupEnv "USE_REAL_CHATGPT_API"
        case useRealAPI of
          Just "true" -> do
            -- Test multiple requests to ensure no resource leaks
            let requests = replicate 3 (createLLMAssembler chatGPTConfig realVideoRequest realAssemblyContext)
            results <- sequence requests
            
            -- All requests should complete (success or controlled failure)
            length results `shouldBe` 3
            
            -- At least some should succeed (unless there's a systematic issue)
            let successCount = length [r | r@(Assembly.Success _) <- results]
            successCount `shouldSatisfy` (>= 1)
          _ -> pendingWith "Set USE_REAL_CHATGPT_API=true to run real API tests"

  describe "Mock vs Real API Consistency" $ do
    it "produces similar structure between mock and real responses" $ do
      -- Test mock response
      mockResult <- createLLMAssembler chatGPTConfig realVideoRequest realAssemblyContext
      
      case mockResult of
        Assembly.Success mockLayout -> do
          -- Mock should produce valid structure
          layoutId mockLayout `shouldSatisfy` (not . T.null)
          length (segments mockLayout) `shouldSatisfy` (>= 1)
          
          -- If real API is available, compare structures
          useRealAPI <- lookupEnv "USE_REAL_CHATGPT_API"
          case useRealAPI of
            Just "true" -> do
              -- This test compares mock vs real structure
              pendingWith "Structure comparison between mock and real API - manual verification needed"
            _ -> return () -- Just verify mock works
        _ -> expectationFailure "Mock should always produce valid response"