module Data.CapabilityGraph.Types
  ( CGraph,
    GNode,
    DirectedGraph,
    UndirectedGraph,
    emptyDGraph,
    emptyUGraph,
    empty,
    order,
    isElement,
    insert,
    nodesByValue,
  )
where

import Data.Foldable (foldl')
import qualified Data.HashMap.Lazy as HM
import qualified Data.Hashable as H
import qualified Data.List as L (filter, nubBy)
import Data.Maybe
import qualified Test.QuickCheck as Q

-- | type wrapper to avoid boolean blindness when directed graph is required
type DirectedGraph h v = (CapabilityGraph h v)

-- | type wrapper to avoid boolean blindness when undirected graph is required
type UndirectedGraph h v = (CapabilityGraph h v)

-- | An individual node independent of any graph graph, including its key, additional attributes (if any), and its neighbors
data Node h v = NodeT {hashKey :: h, attributes :: v, neighbors :: HM.HashMap h (Node h v)}

-- | a node associated with a particular graph
data GNode h v = GNodeT (CapabilityGraph h v) (Node h v)

instance (Show h, Show v) => Show (Node h v) where
  show (NodeT h v n) = "NodeT " ++ show h ++ " " ++ show v ++ " " ++ show n

type CGraph v = CapabilityGraph Int v

-- | The type of a capability graph
newtype CapabilityGraph h v = CapabilityGraphT
  { -- | returns the underlying data structure we're using.
    -- | Should never be used directly by non-library code
    unHashMap :: HM.HashMap h (Node h v)
  }

instance (Show h, Show v) => Show (CapabilityGraph h v) where
  show g = "CapabilityGraphT { " ++ show (unHashMap g) ++ "}"

-- | The empty graph
empty :: (Eq h, H.Hashable h) => CapabilityGraph h v
empty = CapabilityGraphT {unHashMap = HM.empty}

emptyDGraph :: (Eq h, H.Hashable h) => DirectedGraph h v
emptyDGraph = empty

emptyUGraph :: (Eq h, H.Hashable h) => UndirectedGraph h v
emptyUGraph = empty

nodesByValue :: Eq v => CapabilityGraph h v -> v -> [GNode h v]
nodesByValue g v =
  map (GNodeT g) (L.filter (\n -> attributes n == v) (HM.elems (unHashMap g)))

-- | Returns true if a node is an element in the graph
isElement :: (Eq h, H.Hashable h) => CapabilityGraph h v -> GNode h v -> Bool
isElement g (GNodeT _ n) = isJust (HM.lookup (hashKey n) (unHashMap g))

-- | Determines the total number of elements in a graph
order :: (Eq h, H.Hashable h) => CapabilityGraph h v -> Int
order = HM.size . unHashMap

-- | Insert an element into a graph (keeping the old value if present)
insert :: (Eq h, H.Hashable h, Ord h, Num h) => CapabilityGraph h v -> v -> (GNode h v, CapabilityGraph h v)
insert g v =
  let nextId = HM.foldl' (\x y -> maximum [x, hashKey y]) 0 (unHashMap g)
      cgraph = CapabilityGraphT (HM.union (HM.singleton nextId (NodeT nextId v HM.empty)) (unHashMap g))
   in (GNodeT cgraph (NodeT nextId v (maybe HM.empty neighbors (HM.lookup nextId (unHashMap cgraph)))), cgraph)

instance (Q.Arbitrary h, Q.Arbitrary v, H.Hashable h, Eq h) => Q.Arbitrary (Node h v) where
  arbitrary = do
    initNode <- NodeT <$> (Q.arbitrary :: Q.Gen h) <*> (Q.arbitrary :: Q.Gen v) <*> return HM.empty
    hashes <- Q.listOf (Q.arbitrary :: Q.Gen h) `Q.suchThat` \x -> length x <= 10
    nodes <- Q.vectorOf (length hashes) (Q.arbitrary :: Q.Gen v)
    let parents = mconcat [HM.singleton h (NodeT h n HM.empty) | h <- hashes, n <- nodes]
    return (NodeT (hashKey initNode) (attributes initNode) parents)

instance (Q.Arbitrary h, Q.Arbitrary v, H.Hashable h, Eq h) => Q.Arbitrary (CapabilityGraph h v) where
  arbitrary = do
    allNodes <- Q.listOf (Q.arbitrary :: Q.Gen (Node h v)) `Q.suchThat` \x -> length x <= 10
    let nubbedNodes = L.nubBy (\a b -> hashKey a == hashKey b) (concat [an : HM.elems (neighbors an) | an <- allNodes])
    return (CapabilityGraphT (foldl' (\c nxt -> HM.union c (HM.singleton (hashKey nxt) nxt)) HM.empty nubbedNodes))
