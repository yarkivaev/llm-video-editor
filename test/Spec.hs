import Test.Hspec
import qualified VideoAssembler.LLMSpec
import qualified Integration.ChatGPTIntegrationSpec
import qualified Integration.CLISpec
import qualified Integration.CLIEndToEndSpec

main :: IO ()
main = hspec $ do
  describe "VideoAssembler.LLM Unit Tests" VideoAssembler.LLMSpec.spec
  describe "ChatGPT Integration Tests" Integration.ChatGPTIntegrationSpec.spec
  describe "CLI Integration Tests" Integration.CLISpec.spec
  describe "CLI End-to-End Tests" Integration.CLIEndToEndSpec.spec
