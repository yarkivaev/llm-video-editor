{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.LLM
  ( LLM(..)
  ) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)
import Control.Exception (try, SomeException)
import Types.Assembly
import Types.LLMApi

-- | ChatGPT implementation
data LLM = ChatGpt
  deriving (Show, Eq)

-- | LLMApi instance for ChatGPT
instance LLMApi LLM where
  call ChatGpt (Prompt promptText) = do
    -- Get API key from environment
    maybeApiKey <- fmap T.pack <$> lookupEnv "OPENAI_API_KEY"
    
    case maybeApiKey of
      Nothing -> return $ Failure (AssemblyLLMError "No OpenAI API key provided. Set OPENAI_API_KEY environment variable.")
      Just apiKeyValue -> do
        -- Create manager with timeout and connection settings
        let managerSettings = tlsManagerSettings
              { managerConnCount = 1
              , managerResponseTimeout = responseTimeoutMicro (120 * 1000000) -- 120 seconds
              }
        manager <- newManager managerSettings
        
        let llmRequest = LLMRequest
              { llmMessages = [LLMMessage "user" promptText]
              , llmModel = "gpt-3.5-turbo" -- More reliable than gpt-4
              , llmTemperature = 0.7
              , llmMaxTokens = Nothing -- No response limit
              }
        
        let body = encode llmRequest
        let url = "https://api.openai.com/v1/chat/completions"
        
        -- Wrap HTTP call in exception handling
        result <- try $ do
          initialRequest <- parseRequest url
          let request = initialRequest
                { method = "POST"
                , requestHeaders = 
                    [ ("Content-Type", "application/json")
                    , ("Authorization", "Bearer " <> TE.encodeUtf8 apiKeyValue)
                    , ("User-Agent", "llm-video-editor/1.0")
                    ]
                , requestBody = RequestBodyLBS body
                }
          
          response <- httpLbs request manager
          let statusCode' = statusCode $ responseStatus response
          let responseBodyData = responseBody response
          
          if statusCode' == 200
            then case eitherDecode responseBodyData of
              Left err -> return $ Failure (AssemblyLLMError $ T.pack $ "Failed to parse ChatGPT response: " ++ err)
              Right llmResponse -> do
                let extractedContent = case choices llmResponse of
                      [] -> ""
                      (choice:_) -> content $ message choice
                return $ parseResponse extractedContent
            else return $ Failure (AssemblyLLMError $ T.pack $ "ChatGPT API error. Status code: " ++ show statusCode' ++ ". Response: " ++ T.unpack (T.take 500 (T.pack $ show responseBodyData)))
        
        case result of
          Left (ex :: SomeException) -> return $ Failure (AssemblyLLMError $ T.pack $ "Network error connecting to OpenAI API: " ++ show ex)
          Right assemblyResult -> return assemblyResult
