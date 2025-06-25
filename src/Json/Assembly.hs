{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Json.Assembly where

import Types.Assembly
import Types.Common
import Json.Common () -- Import instances for common types
import Json.Video () -- Import instances for video types

import Data.Aeson

instance ToJSON LLMConfig
instance FromJSON LLMConfig

instance ToJSON AssemblyStrategy
instance FromJSON AssemblyStrategy where
  parseJSON = withObject "AssemblyStrategy" $ \o -> do
    strategyType <- o .: "type"
    case strategyType of
      "SingleLLM" -> SingleLLM <$> o .: "config"
      "SequentialLLM" -> SequentialLLM <$> o .: "configs"
      "HierarchicalAssembly" -> HierarchicalAssembly <$> o .: "masterConfig" <*> o .: "detailConfig"
      "EnsembleAssembly" -> EnsembleAssembly <$> o .: "configs"
      "HybridAssembly" -> HybridAssembly <$> o .: "strategy1" <*> o .: "strategy2"
      _ -> fail $ "Unknown assembly strategy: " ++ strategyType

instance ToJSON AssemblyContext
instance FromJSON AssemblyContext where
  parseJSON = withObject "AssemblyContext" $ \o -> AssemblyContext
    <$> o .: "strategy"
    <*> (fmap Duration <$> o .:? "maxVideoDuration")
    <*> o .:? "preferredStyle"
    <*> o .:? "targetAudience"
    <*> o .:? "budgetConstraints"
    <*> o .:? "technicalLimits" .!= []
    <*> o .:? "customInstructions" .!= []

instance ToJSON AssemblyError
instance FromJSON AssemblyError

instance ToJSON AssemblyResult
instance FromJSON AssemblyResult