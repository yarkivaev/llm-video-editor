{-# LANGUAGE OverloadedStrings #-}

module Types.ChatGpt
  ( ChatGpt(..)
  ) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)
import Types.Assembly
import Types.LLM

-- | ChatGPT implementation
data ChatGpt = ChatGpt
  deriving (Show, Eq)

-- | LLM instance for ChatGPT
instance LLM ChatGpt where
  call _ (Prompt promptText) = do
    -- Get API key from environment
    maybeApiKey <- fmap T.pack <$> lookupEnv "OPENAI_API_KEY"
    
    case maybeApiKey of
      Nothing -> return $ Failure (AssemblyLLMError "No OpenAI API key provided. Set OPENAI_API_KEY environment variable.")
      Just apiKeyValue -> do
        manager <- newManager tlsManagerSettings
        
        let llmRequest = LLMRequest
              { llmMessages = [LLMMessage "user" promptText]
              , llmModel = "gpt-4"
              , llmTemperature = 0.7
              , llmMaxTokens = Nothing
              }
        
        let body = encode llmRequest
        let url = "https://api.openai.com/v1/chat/completions"
        
        initialRequest <- parseRequest url
        let request = initialRequest
              { method = "POST"
              , requestHeaders = 
                  [ ("Content-Type", "application/json")
                  , ("Authorization", "Bearer " <> TE.encodeUtf8 apiKeyValue)
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
          else return $ Failure (AssemblyLLMError $ T.pack $ "ChatGPT API error. Status code: " ++ show statusCode' ++ ". Response: " ++ show responseBodyData)