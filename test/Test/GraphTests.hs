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
  prop_isElementAfterEmptyGraphAlwaysFalse
  prop_isElementAfterEmptyDGraphAlwaysFalse
  prop_isElementAfterUpsertEmptyAlwaysTrue
  prop_orderOfGraphEqualsLengthOfnodes

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

prop_isElementAfterEmptyGraphAlwaysFalse :: S.Spec
prop_isElementAfterEmptyGraphAlwaysFalse =
  SQC.prop "isElement over empty always false" $ do
    \(n :: Node Int) -> not (isElement empty n)

prop_isElementAfterEmptyDGraphAlwaysFalse :: S.Spec
prop_isElementAfterEmptyDGraphAlwaysFalse =
  SQC.prop "isElement over empty digraph always false" $ do
    \(n :: Node Int) -> not (isElement emptyDGraph n)

prop_isElementAfterEmptyUGraphAlwaysFalse :: S.Spec
prop_isElementAfterEmptyUGraphAlwaysFalse =
  SQC.prop "isElement over empty undirected graph always false" $ do
    \(n :: Node Int) -> not (isElement emptyUGraph n)

prop_isElementAfterUpsertEmptyAlwaysTrue :: S.Spec
prop_isElementAfterUpsertEmptyAlwaysTrue =
  SQC.prop "isElement over upsert . empty always true" $ do
    \(n :: Node Int) -> isElement (upsert empty n) n

prop_isElementAfterUpsertAlwaysTrue :: S.Spec
prop_isElementAfterUpsertAlwaysTrue =
  SQC.prop "isElement ver upsert always true" $ do
    \(n :: Node Int) (g :: Graph Int) -> isElement (upsert g n)

prop_orderOfGraphEqualsLengthOfnodes :: S.Spec
prop_orderOfGraphEqualsLengthOfnodes =
  SQC.prop "order of graph equals length of its node list" $ do
    \(g :: Graph Int) -> order g == length (nodes g)
