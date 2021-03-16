import Test.GraphSpec (graphSpec)
import Test.GraphTests (graphTestSpec)
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "graph API behaves correctly in constructed specs" $ do
    graphSpec
  describe "graph API behaves correctly w.r.t. QC properties" $ do
    graphTestSpec
