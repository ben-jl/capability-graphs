module Test.GraphTests where

import Data.CapabilityGraph
import qualified Data.List as L (nub, subsequences)
import Data.Maybe (isNothing)
import qualified Test.Hspec as S
import qualified Test.Hspec.QuickCheck as SQC
import Test.QuickCheck.Instances.Text ()

graphTestSpec :: S.Spec
graphTestSpec = do
  prop_orderOfAnEmptyGraphAlwaysZero
  prop_isElementAfterEmptyGraphAlwaysFalse
  prop_isElementAfterInsertEmptyAlwaysTrue
  prop_orderOfGraphMadeFromKeyedSameAsNumberOfDistinctEls
  prop_orderOverInsertNewlyWrappedContextEqualsOne
  prop_adjacentAfterCreateEdgeAlwaysTrue
  prop_adjacentAfterEmptyAlwaysFalse
  prop_parentsAfterEmptyAlwaysEmpty
  prop_allAncestorsAfterEmptyAlwaysJustNode
  prop_leastCommonAncestorOfEmptyAlwaysNothing

prop_orderOfAnEmptyGraphAlwaysZero :: S.Spec
prop_orderOfAnEmptyGraphAlwaysZero =
  SQC.prop "order over empty always zero" $ do
    \(_ :: Int) -> order empty == 0

prop_isElementAfterEmptyGraphAlwaysFalse :: S.Spec
prop_isElementAfterEmptyGraphAlwaysFalse =
  SQC.prop "isElement over empty always false" $ do
    \(n :: Node Int) -> not (isElement empty n)

prop_isElementAfterInsertEmptyAlwaysTrue :: S.Spec
prop_isElementAfterInsertEmptyAlwaysTrue =
  SQC.prop "isElement over insert . empty always true" $ do
    \(n :: Node Int) -> isElement (insert empty n) n

prop_orderOfGraphMadeFromKeyedSameAsNumberOfDistinctEls :: S.Spec
prop_orderOfGraphMadeFromKeyedSameAsNumberOfDistinctEls =
  SQC.prop "Order of graph made from dijoint unions equals total number of elements in list" $ do
    \(ns :: [Int]) ->
      let zipped = zip (L.nub [x | x <- ns]) (take (length (L.nub ns)) (L.subsequences [x | x <- ns]))
       in order (withHashFunction id zipped) == length (L.nub ns)

prop_orderOverInsertNewlyWrappedContextEqualsOne :: S.Spec
prop_orderOverInsertNewlyWrappedContextEqualsOne =
  SQC.prop "Order of a graph after inserting element differs by exactly 1 from the original value" $ do
    \(ns :: [Int]) (n :: Int) ->
      let zipped = zip (L.nub [x | x <- ns]) (take (length (L.nub ns)) (L.subsequences [x | x <- ns]))
          graph = withHashFunction id zipped
       in abs (order graph - order (insert graph (wrapNode (\x -> (1 + maximum (x : ns))) n))) == 1

prop_adjacentAfterCreateEdgeAlwaysTrue :: S.Spec
prop_adjacentAfterCreateEdgeAlwaysTrue =
  SQC.prop "Adjacent after create edge always true" $ do
    \(n1 :: Int) (n2 :: Int) (g :: Graph Int) ->
      let wrapped1 = wrapNode id n1
          wrapped2 = wrapNode id n2
       in adjacent (createEdge g wrapped1 wrapped2) wrapped1 wrapped2

prop_adjacentAfterEmptyAlwaysFalse :: S.Spec
prop_adjacentAfterEmptyAlwaysFalse =
  SQC.prop "Adjacent after empty always true" $ do
    \(n1 :: Int) (n2 :: Int) -> not (adjacent empty (wrapNode id n1) (wrapNode id n2))

prop_parentsAfterEmptyAlwaysEmpty :: S.Spec
prop_parentsAfterEmptyAlwaysEmpty =
  SQC.prop "Parents after empty always empty" $ do
    \(n1 :: Int) -> null (parents empty (wrapNode id n1))

prop_allAncestorsAfterEmptyAlwaysJustNode :: S.Spec
prop_allAncestorsAfterEmptyAlwaysJustNode =
  SQC.prop "All ancestors after empty always contains just node" $ do
    \(n1 :: Int) -> length (allAncestors 0 empty (wrapNode id n1)) == 1

prop_leastCommonAncestorOfEmptyAlwaysNothing :: S.Spec
prop_leastCommonAncestorOfEmptyAlwaysNothing =
  SQC.prop "Least common ancestor of empty always Nothing" $ do
    \(n1 :: Node Int) (n2 :: Node Int) -> isNothing (leastCommonAncestor empty n1 n2)
