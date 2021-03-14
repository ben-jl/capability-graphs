import Test.GraphTests (graphTestSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  graphTestSpec
