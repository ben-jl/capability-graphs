module Test.GraphTests (graphTestSpec) where

import Data.CapabilityGraph
import qualified Data.Text as T
import qualified Test.Hspec as S
import qualified Test.Hspec.QuickCheck as SQC
import Test.QuickCheck.Instances.Text ()

graphTestSpec :: S.Spec
graphTestSpec = do
  prop_orderOfAnEmptyGraphAlwaysZero
  prop_nodesByValueAfterInsertGTOne
  prop_orderOfEmptyDGraphAlwaysZero
  prop_orderOfEmptyUGraphAlwaysZero
  prop_isElementAfterInsertAlwaysTrue

prop_orderOfAnEmptyGraphAlwaysZero :: S.Spec
prop_orderOfAnEmptyGraphAlwaysZero =
  SQC.prop "order over empty always zero" $ do
    \(_ :: Int) -> order (empty :: CGraph T.Text) == 0

prop_nodesByValueAfterInsertGTOne :: S.Spec
prop_nodesByValueAfterInsertGTOne =
  SQC.prop "nodesByValue over insert always has length of at least one" $ do
    \(g :: CGraph T.Text, n :: T.Text) ->
      not (null (nodesByValue (snd (insert g n)) n))

prop_orderOfEmptyDGraphAlwaysZero :: S.Spec
prop_orderOfEmptyDGraphAlwaysZero =
  SQC.prop "order over emptyDGraph always zero" $ do
    \(_ :: Int) -> order (emptyDGraph :: DirectedGraph Int T.Text) == 0

prop_orderOfEmptyUGraphAlwaysZero :: S.Spec
prop_orderOfEmptyUGraphAlwaysZero =
  SQC.prop "order over emptyUGraph always zero" $ do
    \(_ :: Int) -> order (emptyUGraph :: UndirectedGraph Int T.Text) == 0

prop_isElementAfterInsertAlwaysTrue :: S.Spec
prop_isElementAfterInsertAlwaysTrue =
  SQC.prop "isElement over insert always true" $ do
    \g n ->
      let (node, graph) = insert (g :: CGraph T.Text) n
       in isElement graph node
