module Test.GraphSpec (graphSpec) where

import Data.CapabilityGraph
import qualified Data.Hashable as H
import qualified Test.Hspec as S

graphSpec :: S.Spec
graphSpec = do
  S.describe "can construct and query a simple graph" $ do
    basicAncestrySpec
  S.describe "can construct and query a history that reconnects" $ do
    basicBranchingHistorySpec

basicAncestrySpec :: S.Spec
basicAncestrySpec =
  -- 1 -> 2 -> 4 -> []
  --     \
  --      -> 3 -> []
  let graph = withHashFunction id ([(1 :: Int, [2 :: Int, 3 :: Int]), (2, [4]), (3, []), (4, [])] :: [(Int, [Int])])
   in do
        S.it "has order of 4 after construction" $ do
          order graph == 4
        S.it "1 is adjacent to 2" $ do
          adjacent graph (wrapNode H.hash 1) (wrapNode H.hash 2)
        S.it "1 is adjacent to 3" $ do
          adjacent graph (wrapNode H.hash 1) (wrapNode H.hash 3)
        S.it "2 is adjacent to 4" $ do
          adjacent graph (wrapNode H.hash 2) (wrapNode H.hash 4)

basicBranchingHistorySpec :: S.Spec
basicBranchingHistorySpec =
  -- 1 -> 2 -> 6 -> 7 -> []
  --        \     /
  -- 3 -> 4 -> 5 -> []
  let graph = withHashFunction id [(1 :: Int, [2 :: Int]), (2, [5, 6]), (3, [4]), (4, [5]), (5, [7]), (6, [7])]
   in do
        S.it "has order 7 after construction" $ do
          order graph == 7
        S.it "1 is adjacent to 2" $ do
          adjacent graph (wrapNode H.hash 1) (wrapNode H.hash 2)
        S.it "2 is adjacent to 5" $ do
          adjacent graph (wrapNode H.hash 2) (wrapNode H.hash 5)
        S.it "3 is adjacent to 4" $ do
          adjacent graph (wrapNode H.hash 3) (wrapNode H.hash 4)
        S.it "4 is adjacent to 5" $ do
          adjacent graph (wrapNode H.hash 4) (wrapNode H.hash 5)
        S.it "2 and 6 have least common ancestor 6" $ do
          leastCommonAncestor graph (wrapNode H.hash 2) (wrapNode H.hash 6) == Just (wrapNode H.hash 6)
        S.it "1 and 3 have least common ancestor 5" $ do
          leastCommonAncestor graph (wrapNode H.hash 1) (wrapNode H.hash 3) == Just (wrapNode H.hash 5)
        S.it "6 and 3 have least common ancestor 7" $ do
          leastCommonAncestor graph (wrapNode H.hash 6) (wrapNode H.hash 3) == Just (wrapNode H.hash 7)
        S.it "2 and 4 have least common ancestor 5" $ do
          leastCommonAncestor graph (wrapNode H.hash 2) (wrapNode H.hash 4) == Just (wrapNode H.hash 5)
