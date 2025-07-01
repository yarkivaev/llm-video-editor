{-# LANGUAGE OverloadedStrings #-}

module Types.AssemblySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson (encode, decode)
import Data.Time (parseTimeOrError, defaultTimeLocale)
import qualified Types.Assembly as Assembly
import Types.Assembly (AssemblyContext(..), AssemblyError(..), AssemblyStrategy(..), LLMConfig(..))
import Types.Common
import Types.Video

-- Test data
sampleTimestamp :: Timestamp
sampleTimestamp = Timestamp $ parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2024-01-01 12:00:00 UTC"

sampleLLMConfig :: LLMConfig
sampleLLMConfig = LLMConfig
  { modelName = "gpt-4"
  , temperature = 0.7
  , maxTokens = Just 2000
  , systemPrompt = Nothing
  , apiEndpoint = Nothing
  , apiKey = Nothing
  }

sampleAssemblyContext :: AssemblyContext
sampleAssemblyContext = AssemblyContext
  { strategy = SingleLLM sampleLLMConfig
  , maxVideoDuration = Just (Duration 120.0)
  , preferredStyle = Just "cinematic"
  , targetAudience = Just "family"
  , budgetConstraints = Just "low"
  , technicalLimits = ["max_segments_10", "no_complex_transitions"]
  , customInstructions = ["focus_on_happy_moments", "include_sunset_photo"]
  }

sampleVideoLayout :: VideoLayout
sampleVideoLayout = VideoLayout
  { layoutId = "test-layout-1"
  , totalDuration = Duration 120.5
  , segments = []
  , globalAudio = []
  , outputFormat = "mp4"
  , outputResolution = Resolution 1920 1080
  , outputFrameRate = 30.0
  , layoutCreatedAt = sampleTimestamp
  }

spec :: Spec
spec = do
  describe "LLMConfig" $ do
    it "serializes and deserializes correctly" $ do
      let config = LLMConfig "gpt-4" 0.8 (Just 1500) (Just "system") (Just "https://api.test.com") (Just "key123")
      decode (encode config) `shouldBe` Just config
    
    it "handles optional fields correctly" $ do
      let minimalConfig = LLMConfig "claude-3" 0.5 Nothing Nothing Nothing Nothing
      decode (encode minimalConfig) `shouldBe` Just minimalConfig

  describe "AssemblyStrategy" $ do
    it "serializes SingleLLM strategy correctly" $ do
      let strategy = SingleLLM sampleLLMConfig
      decode (encode strategy) `shouldBe` Just strategy
    
    it "serializes SequentialLLM strategy correctly" $ do
      let strategy = SequentialLLM [sampleLLMConfig, sampleLLMConfig { modelName = "claude-3" }]
      decode (encode strategy) `shouldBe` Just strategy
    
    it "serializes HierarchicalAssembly strategy correctly" $ do
      let strategy = HierarchicalAssembly sampleLLMConfig (sampleLLMConfig { temperature = 0.9 })
      decode (encode strategy) `shouldBe` Just strategy
    
    it "serializes EnsembleAssembly strategy correctly" $ do
      let strategy = EnsembleAssembly [sampleLLMConfig, sampleLLMConfig { modelName = "claude-3" }]
      decode (encode strategy) `shouldBe` Just strategy
    
    it "serializes HybridAssembly strategy correctly" $ do
      let strategy1 = SingleLLM sampleLLMConfig
      let strategy2 = SequentialLLM [sampleLLMConfig]
      let hybridStrategy = HybridAssembly strategy1 strategy2
      decode (encode hybridStrategy) `shouldBe` Just hybridStrategy

  describe "AssemblyContext" $ do
    it "serializes and deserializes correctly" $ do
      decode (encode sampleAssemblyContext) `shouldBe` Just sampleAssemblyContext
    
    it "handles empty lists correctly" $ do
      let contextWithEmptyLists = sampleAssemblyContext
            { technicalLimits = []
            , customInstructions = []
            }
      decode (encode contextWithEmptyLists) `shouldBe` Just contextWithEmptyLists
    
    it "handles optional fields correctly" $ do
      let minimalContext = AssemblyContext
            { strategy = SingleLLM sampleLLMConfig
            , maxVideoDuration = Nothing
            , preferredStyle = Nothing
            , targetAudience = Nothing
            , budgetConstraints = Nothing
            , technicalLimits = []
            , customInstructions = []
            }
      decode (encode minimalContext) `shouldBe` Just minimalContext

  describe "AssemblyError" $ do
    it "serializes different error types correctly" $ do
      let errors = 
            [ AssemblyLLMError "API failed"
            , InvalidPrompt "Empty prompt"
            , AssemblyInsufficientMedia "Need more videos"
            , TechnicalConstraintViolation "Too many segments"
            , AssemblyTimeoutError (Duration 30.0)
            , AssemblyParseError "Invalid JSON"
            , AssemblyValidationError "Missing required field"
            ]
      mapM_ (\err -> decode (encode err) `shouldBe` Just err) errors

  describe "AssemblyResult" $ do
    it "serializes Success result correctly" $ do
      let result = Assembly.Success sampleVideoLayout
      decode (encode result) `shouldBe` Just result
    
    it "serializes Failure result correctly" $ do
      let result = Assembly.Failure (AssemblyLLMError "Test error")
      decode (encode result) `shouldBe` Just result
    
    it "serializes PartialSuccess result correctly" $ do
      let result = Assembly.PartialSuccess sampleVideoLayout ["Warning 1", "Warning 2"]
      decode (encode result) `shouldBe` Just result

  describe "JSON compatibility" $ do
    context "property tests" $ do
      it "round-trip serialization preserves data" $ property $ \temp ->
        let config = LLMConfig "test-model" (abs temp) Nothing Nothing Nothing Nothing
            strategy = SingleLLM config
        in decode (encode strategy) === Just strategy