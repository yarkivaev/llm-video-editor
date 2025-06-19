import Test.Hspec
import qualified VideoAssembler.LLMSpec
import qualified Integration.ChatGPTIntegrationSpec
import qualified Integration.CLISpec
import qualified Integration.CLIEndToEndSpec
import qualified VideoRendererSpec
import qualified VideoExporterSpec
import qualified Integration.VideoRendererIntegrationSpec
import qualified Integration.VideoExporterEndToEndSpec

main :: IO ()
main = hspec $ do
  describe "VideoAssembler.LLM Unit Tests" VideoAssembler.LLMSpec.spec
  describe "ChatGPT Integration Tests" Integration.ChatGPTIntegrationSpec.spec
  describe "CLI Integration Tests" Integration.CLISpec.spec
  describe "CLI End-to-End Tests" Integration.CLIEndToEndSpec.spec
  describe "VideoRenderer Unit Tests" VideoRendererSpec.spec
  describe "VideoExporter Unit Tests" VideoExporterSpec.spec
  describe "VideoRenderer Integration Tests" Integration.VideoRendererIntegrationSpec.spec
  describe "VideoExporter End-to-End Tests" Integration.VideoExporterEndToEndSpec.spec
