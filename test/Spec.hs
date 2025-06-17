import Test.Hspec
import qualified VideoAssembler.LLMSpec

main :: IO ()
main = hspec $ do
  describe "VideoAssembler.LLM" VideoAssembler.LLMSpec.spec
