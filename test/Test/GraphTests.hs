module Test.GraphTests (graphTestSpec) where

import Data.CapabilityGraph
import qualified Data.Text as T
import qualified Test.Hspec as S
import qualified Test.Hspec.QuickCheck as SQC
import Test.QuickCheck.Instances.Text ()

graphTestSpec :: S.Spec
graphTestSpec = do
  prop_orderOfAnEmptyGraphAlwaysZero
  prop_isElementAfterEmptyGraphAlwaysFalse
  prop_isElementAfterUpsertEmptyAlwaysTrue

prop_orderOfAnEmptyGraphAlwaysZero :: S.Spec
prop_orderOfAnEmptyGraphAlwaysZero =
  SQC.prop "order over empty always zero" $ do
    \(_ :: Int) -> order (empty) == 0

prop_isElementAfterEmptyGraphAlwaysFalse :: S.Spec
prop_isElementAfterEmptyGraphAlwaysFalse =
  SQC.prop "isElement over empty always false" $ do
    \(n :: Node Int) -> not (isElement empty n)

prop_isElementAfterUpsertEmptyAlwaysTrue :: S.Spec
prop_isElementAfterUpsertEmptyAlwaysTrue =
  SQC.prop "isElement over upsert . empty always true" $ do
    \(n :: Node Int) -> isElement (upsert empty n) n
