{-# LANGUAGE OverloadedStrings #-}

module VideoAssembler.LLMSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)
import VideoAssembler.LLM
import qualified Types.Assembly as Assembly
import Types

-- Test data
sampleTimestamp :: Timestamp
sampleTimestamp = Timestamp $ parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2024-01-01 12:00:00 UTC"

sampleVideoFile :: VideoFile
sampleVideoFile = VideoFile
  { videoMetadata = MediaMetadata
      { fileName = "vacation_clip.mp4"
      , filePath = "/path/to/vacation_clip.mp4"
      , fileSize = 1048576
      , createdAt = sampleTimestamp
      , location = Just $ Location 45.5017 (-73.5673) (Just "Montreal, QC")
      , description = Just "Beach vacation highlights"
      , tags = ["vacation", "beach", "summer"]
      }
  , videoDuration = Duration 120.0
  , resolution = Resolution 1920 1080
  , frameRate = 30.0
  , hasAudio = True
  , videoFormat = "mp4"
  , contentAnalysis = Just $ VideoContentAnalysis
      { contentOverview = "Beautiful beach scenes with family activities"
      , actionIntroduction = "Shows family building sandcastles and swimming"
      , timeBoundDetails = 
          [ TimeBoundDetail (Duration 0.0) (Duration 30.0) "Family arrives at beach" (Just 0.95)
          , TimeBoundDetail (Duration 30.0) (Duration 60.0) "Building sandcastles" (Just 0.88)
          , TimeBoundDetail (Duration 60.0) (Duration 120.0) "Swimming and playing" (Just 0.92)
          ]
      , detectedObjects = ["people", "beach", "ocean", "sandcastles"]
      , detectedScenes = ["outdoor", "beach", "sunny"]
      , estimatedMood = Just "joyful"
      }
  }

samplePhotoFile :: PhotoFile
samplePhotoFile = PhotoFile
  { photoMetadata = MediaMetadata
      { fileName = "sunset.jpg"
      , filePath = "/path/to/sunset.jpg"
      , fileSize = 2048576
      , createdAt = sampleTimestamp
      , location = Just $ Location 45.5017 (-73.5673) (Just "Montreal, QC")
      , description = Just "Beautiful sunset"
      , tags = ["sunset", "landscape"]
      }
  , photoResolution = Resolution 3840 2160
  , imageFormat = "jpg"
  , cameraSettings = Just "ISO 100, f/8, 1/125s"
  }

sampleVideoRequest :: VideoRequest
sampleVideoRequest = VideoRequest
  { requestId = "test-request-1"
  , mediaFiles = [Video sampleVideoFile, Photo samplePhotoFile]
  , userPrompt = "Create a 2-minute vacation highlight reel with upbeat music and smooth transitions"
  , submittedAt = sampleTimestamp
  }

sampleAssemblyContext :: AssemblyContext
sampleAssemblyContext = AssemblyContext
  { strategy = SingleLLM $ LLMConfig "gpt-4" 0.7 (Just 2000) Nothing Nothing Nothing
  , maxVideoDuration = Just (Duration 120.0)
  , preferredStyle = Just "cinematic"
  , targetAudience = Just "family"
  , budgetConstraints = Just "low"
  , technicalLimits = ["max_segments_10", "no_complex_transitions"]
  , customInstructions = ["focus_on_happy_moments", "include_sunset_photo"]
  }

spec :: Spec
spec = do
  describe "generatePrompt" $ do
    it "includes user prompt in generated text" $ do
      let prompt = generatePrompt sampleVideoRequest sampleAssemblyContext
      prompt `shouldSatisfy` T.isInfixOf (userPrompt sampleVideoRequest)
    
    it "includes media file information" $ do
      let prompt = generatePrompt sampleVideoRequest sampleAssemblyContext
      prompt `shouldSatisfy` T.isInfixOf "vacation_clip.mp4"
      prompt `shouldSatisfy` T.isInfixOf "sunset.jpg"
    
    it "includes content analysis when available" $ do
      let prompt = generatePrompt sampleVideoRequest sampleAssemblyContext
      prompt `shouldSatisfy` T.isInfixOf "Beautiful beach scenes with family activities"
    
    it "includes technical constraints" $ do
      let prompt = generatePrompt sampleVideoRequest sampleAssemblyContext
      prompt `shouldSatisfy` T.isInfixOf "max_segments_10"
      prompt `shouldSatisfy` T.isInfixOf "no_complex_transitions"
    
    it "includes assembly context preferences" $ do
      let prompt = generatePrompt sampleVideoRequest sampleAssemblyContext
      prompt `shouldSatisfy` T.isInfixOf "cinematic"
      prompt `shouldSatisfy` T.isInfixOf "family"
      prompt `shouldSatisfy` T.isInfixOf "120"
    
    it "includes custom instructions" $ do
      let prompt = generatePrompt sampleVideoRequest sampleAssemblyContext
      prompt `shouldSatisfy` T.isInfixOf "focus_on_happy_moments"
      prompt `shouldSatisfy` T.isInfixOf "include_sunset_photo"
    
    it "includes JSON structure example" $ do
      let prompt = generatePrompt sampleVideoRequest sampleAssemblyContext
      prompt `shouldSatisfy` T.isInfixOf "layoutId"
      prompt `shouldSatisfy` T.isInfixOf "segments"
      prompt `shouldSatisfy` T.isInfixOf "outputFormat"
    
    it "handles empty media files gracefully" $ do
      let emptyRequest = sampleVideoRequest { mediaFiles = [] }
      let prompt = generatePrompt emptyRequest sampleAssemblyContext
      prompt `shouldSatisfy` (not . T.null)
    
    it "handles missing content analysis" $ do
      let videoWithoutAnalysis = sampleVideoFile { contentAnalysis = Nothing }
      let requestWithoutAnalysis = sampleVideoRequest 
            { mediaFiles = [Video videoWithoutAnalysis] }
      let prompt = generatePrompt requestWithoutAnalysis sampleAssemblyContext
      prompt `shouldSatisfy` T.isInfixOf "vacation_clip.mp4"
      prompt `shouldSatisfy` (not . T.isInfixOf "Beautiful beach scenes")
    
    it "handles empty technical limits" $ do
      let contextWithoutLimits = sampleAssemblyContext { technicalLimits = [] }
      let prompt = generatePrompt sampleVideoRequest contextWithoutLimits
      prompt `shouldSatisfy` (not . T.null)
    
    context "property tests" $ do
      it "prompt length is reasonable" $ do
        let prompt = generatePrompt sampleVideoRequest sampleAssemblyContext
        T.length prompt `shouldSatisfy` (> 100)
        T.length prompt `shouldSatisfy` (< 10000)

  describe "parseResponse" $ do
    it "parses valid JSON response successfully" $ do
      let validJson = T.unlines
            [ "{"
            , "  \"layoutId\": \"test-layout-1\","
            , "  \"totalDuration\": 120.5,"
            , "  \"segments\": [],"
            , "  \"globalAudio\": [],"
            , "  \"outputFormat\": \"mp4\","
            , "  \"outputResolution\": {\"width\": 1920, \"height\": 1080},"
            , "  \"outputFrameRate\": 30.0"
            , "}"
            ]
      case parseResponse validJson of
        Assembly.Success layout -> do
          layoutId layout `shouldBe` "test-layout-1"
          totalDuration layout `shouldBe` Duration 120.5
          outputFormat layout `shouldBe` "mp4"
        _ -> expectationFailure "Expected Success but got failure"
    
    it "handles malformed JSON gracefully" $ do
      let invalidJson = "{ invalid json }"
      case parseResponse invalidJson of
        Assembly.Failure (AssemblyParseError _) -> return ()
        _ -> expectationFailure "Expected AssemblyParseError"
    
    it "handles missing required fields" $ do
      let incompleteJson = "{\"layoutId\": \"test\"}"
      case parseResponse incompleteJson of
        Assembly.Failure (AssemblyParseError _) -> return ()
        _ -> expectationFailure "Expected AssemblyParseError for incomplete JSON"
    
    it "parses complex layout with segments" $ do
      let complexJson = T.unlines
            [ "{"
            , "  \"layoutId\": \"complex-layout\","
            , "  \"totalDuration\": 60.0,"
            , "  \"segments\": ["
            , "    {"
            , "      \"segmentId\": \"seg-1\","
            , "      \"segmentType\": {"
            , "        \"type\": \"VideoClip\","
            , "        \"mediaId\": \"video-1\","
            , "        \"startTime\": 0.0,"
            , "        \"endTime\": 30.0,"
            , "        \"playbackSpeed\": 1.0"
            , "      },"
            , "      \"segmentStart\": 0.0,"
            , "      \"segmentEnd\": 30.0,"
            , "      \"textOverlays\": [],"
            , "      \"audioTracks\": [],"
            , "      \"transition\": null"
            , "    }"
            , "  ],"
            , "  \"globalAudio\": [],"
            , "  \"outputFormat\": \"mp4\","
            , "  \"outputResolution\": {\"width\": 1920, \"height\": 1080},"
            , "  \"outputFrameRate\": 30.0"
            , "}"
            ]
      case parseResponse complexJson of
        Assembly.Success layout -> do
          length (segments layout) `shouldBe` 1
          let segment = head (segments layout)
          segmentId segment `shouldBe` "seg-1"
        _ -> expectationFailure "Expected Success for complex layout"
    
    it "parses different segment types correctly" $ do
      let jsonWithTitleCard = T.unlines
            [ "{"
            , "  \"layoutId\": \"title-layout\","
            , "  \"totalDuration\": 10.0,"
            , "  \"segments\": ["
            , "    {"
            , "      \"segmentId\": \"title-1\","
            , "      \"segmentType\": {"
            , "        \"type\": \"TitleCard\","
            , "        \"text\": \"My Vacation\","
            , "        \"duration\": 5.0"
            , "      },"
            , "      \"segmentStart\": 0.0,"
            , "      \"segmentEnd\": 5.0,"
            , "      \"textOverlays\": [],"
            , "      \"audioTracks\": [],"
            , "      \"transition\": null"
            , "    }"
            , "  ],"
            , "  \"globalAudio\": [],"
            , "  \"outputFormat\": \"mp4\","
            , "  \"outputResolution\": {\"width\": 1920, \"height\": 1080},"
            , "  \"outputFrameRate\": 30.0"
            , "}"
            ]
      case parseResponse jsonWithTitleCard of
        Assembly.Success layout -> do
          let segment = head (segments layout)
          case segmentType segment of
            TitleCard text dur -> do
              text `shouldBe` "My Vacation"
              dur `shouldBe` Duration 5.0
            _ -> expectationFailure "Expected TitleCard segment type"
        _ -> expectationFailure "Expected Success for title card layout"
    
    context "property tests" $ do
      it "parsing never crashes on empty input" $ 
        case parseResponse "" of
          Assembly.Success _ -> True
          Assembly.Failure _ -> True
          Assembly.PartialSuccess _ _ -> True
        `shouldBe` True

  describe "formatMediaFiles" $ do
    it "formats video files with duration and analysis" $ do
      let formatted = formatMediaFiles [Video sampleVideoFile]
      formatted `shouldSatisfy` T.isInfixOf "vacation_clip.mp4"
      formatted `shouldSatisfy` T.isInfixOf "120"
      formatted `shouldSatisfy` T.isInfixOf "Beautiful beach scenes"
    
    it "formats photo files with resolution" $ do
      let formatted = formatMediaFiles [Photo samplePhotoFile]
      formatted `shouldSatisfy` T.isInfixOf "sunset.jpg"
      formatted `shouldSatisfy` T.isInfixOf "3840"
      formatted `shouldSatisfy` T.isInfixOf "2160"
    
    it "handles mixed media types" $ do
      let formatted = formatMediaFiles [Video sampleVideoFile, Photo samplePhotoFile]
      formatted `shouldSatisfy` T.isInfixOf "vacation_clip.mp4"
      formatted `shouldSatisfy` T.isInfixOf "sunset.jpg"

  describe "formatConstraints" $ do
    it "includes all constraint types" $ do
      let formatted = formatConstraints sampleAssemblyContext
      formatted `shouldSatisfy` T.isInfixOf "120"
      formatted `shouldSatisfy` T.isInfixOf "cinematic"
      formatted `shouldSatisfy` T.isInfixOf "family"
      formatted `shouldSatisfy` T.isInfixOf "max_segments_10"
      formatted `shouldSatisfy` T.isInfixOf "focus_on_happy_moments"
    
    it "handles optional fields gracefully" $ do
      let minimalContext = AssemblyContext
            { strategy = SingleLLM $ LLMConfig "gpt-4" 0.7 Nothing Nothing Nothing Nothing
            , maxVideoDuration = Nothing
            , preferredStyle = Nothing
            , targetAudience = Nothing
            , budgetConstraints = Nothing
            , technicalLimits = []
            , customInstructions = []
            }
      let formatted = formatConstraints minimalContext
      formatted `shouldSatisfy` T.isInfixOf "unlimited"
      formatted `shouldSatisfy` T.isInfixOf "any"
      formatted `shouldSatisfy` T.isInfixOf "general"