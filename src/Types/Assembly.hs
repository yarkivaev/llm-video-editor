{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Assembly
  ( AssemblyContext (..)
  , AssemblyResult (..)
  , AssemblyError (..)
  , AssemblyStrategy (..)
  , LLMConfig (..)
  , VideoAssembler (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Common (Duration (..))
import Types.Video (VideoLayout)
import Types.Media

-- | Configuration for LLM-based assembly
data LLMConfig = LLMConfig
  { modelName       :: Text -- e.g., "gpt-4", "claude-3"
  , temperature     :: Double -- creativity level (0.0 to 1.0)
  , maxTokens       :: Maybe Int
  , systemPrompt    :: Maybe Text
  , apiEndpoint     :: Maybe Text
  , apiKey          :: Maybe Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Different strategies for video assembly
data AssemblyStrategy
  = SingleLLM LLMConfig -- single LLM call
  | SequentialLLM [LLMConfig] -- sequence of LLM calls
  | HierarchicalAssembly LLMConfig LLMConfig -- high-level then detailed assembly
  | EnsembleAssembly [LLMConfig] -- multiple LLMs, merge results
  | HybridAssembly AssemblyStrategy AssemblyStrategy -- combine strategies
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Additional context for video assembly
data AssemblyContext = AssemblyContext
  { strategy         :: AssemblyStrategy
  , maxVideoDuration :: Maybe Duration
  , preferredStyle   :: Maybe Text
  , targetAudience   :: Maybe Text
  , budgetConstraints :: Maybe Text
  , technicalLimits  :: [Text] -- e.g., ["no_transitions", "max_segments_10"]
  , customInstructions :: [Text]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Errors that can occur during assembly
data AssemblyError
  = AssemblyLLMError Text -- LLM API or processing error
  | InvalidPrompt Text -- user prompt issues
  | AssemblyInsufficientMedia Text -- not enough media for request
  | TechnicalConstraintViolation Text -- violates technical limits
  | AssemblyTimeoutError Duration -- assembly took too long
  | AssemblyParseError Text -- failed to parse LLM output
  | AssemblyValidationError Text -- generated layout is invalid
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Result of video assembly process
data AssemblyResult
  = Success VideoLayout
  | Failure AssemblyError
  | PartialSuccess VideoLayout [Text] -- layout with warnings
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- -- | Typeclass for video assembly implementations
class Monad m => VideoAssembler m where
  assembleVideo :: VideoRequest -> AssemblyContext -> m AssemblyResult
