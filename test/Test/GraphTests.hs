module Test.GraphTests (graphTestSpec) where

import Data.CapabilityGraph
import qualified Data.Text as T
import qualified Test.Hspec as S
import qualified Test.Hspec.QuickCheck as SQC
import Test.QuickCheck.Instances.Text ()

graphTestSpec :: S.Spec
graphTestSpec = do
  prop_orderOfAnEmptyGraphAlwaysZero
  prop_orderOfEmptyDGraphAlwaysZero
  prop_orderOfEmptyUGraphAlwaysZero

--   prop_nodesByValueAfterInsertGTOne
--   prop_orderOfEmptyDGraphAlwaysZero
--   prop_orderOfEmptyUGraphAlwaysZero
--   prop_isElementAfterInsertAlwaysTrue
--   prop_orderAfterInsertDiffersByAtMostOne

prop_orderOfAnEmptyGraphAlwaysZero :: S.Spec
prop_orderOfAnEmptyGraphAlwaysZero =
  SQC.prop "order over empty always zero" $ do
    \(_ :: Int) -> order (empty :: Graph Int) == 0

prop_orderOfEmptyDGraphAlwaysZero :: S.Spec
prop_orderOfEmptyDGraphAlwaysZero =
  SQC.prop "order over emptyDGraph always zero" $ do
    \(_ :: Int) -> order (emptyDGraph :: DirectedGraph Int) == 0

prop_orderOfEmptyUGraphAlwaysZero :: S.Spec
prop_orderOfEmptyUGraphAlwaysZero =
  SQC.prop "order over emptyUGraph always zero" $ do
    \(_ :: Int) -> order (emptyUGraph :: UndirectedGraph Int) == 0

-- prop_isElementAfterInsertAlwaysTrue :: S.Spec
-- prop_isElementAfterInsertAlwaysTrue =
--   SQC.prop "isElement over insert always true" $ do
--     \g n ->
--       let (node, graph) = insert (g :: CGraph T.Text) n
--        in isElement graph node

-- prop_orderAfterInsertDiffersByAtMostOne :: S.Spec
-- prop_orderAfterInsertDiffersByAtMostOne =
--   SQC.prop "order over insert differs from before insert by at most one" $ do
--     \g n ->
--       let before = order (g :: CGraph T.Text)
--           after = order (snd (insert g n))
--        in abs (before - after) <= 1
