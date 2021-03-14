{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.CapabilityGraph.Types
  ( Graph,
    KeyedByInt,
    DirectedGraph,
    UndirectedGraph,
    emptyDGraph,
    emptyUGraph,
    empty,
    order,
    wrap,
  )
where

import qualified Data.Bifunctor as BF
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Hashable as H
import qualified Test.QuickCheck as Q

data Directed = DirectedT

data Undirected = UndirectedT

type Graph v = (GraphM () v)

type DirectedGraph v = (GraphM Directed v)

type UndirectedGraph v = (GraphM Undirected v)

data Node v = NodeT {key :: Int, unNode :: v}

instance Functor Node where
  f `fmap` n = NodeT {key = key n, unNode = f (unNode n)}

instance (Show v) => Show (Node v) where
  show (NodeT h v) = "NodeT { " ++ show h ++ " " ++ show v ++ " }"

-- | The type of a capability graph
newtype GraphM a v = GraphMT
  { -- | returns the underlying data structure we're using.
    -- | Should never be used directly by non-library code
    unHashMap :: HM.HashMap Int (v, HS.HashSet Int)
  }

instance Functor (GraphM a) where
  f `fmap` g = GraphMT (HM.map (BF.first f) (unHashMap g))

class KeyedByInt a where
  keyedByInt :: a -> Int

instance KeyedByInt (Node v) where
  keyedByInt = key

instance KeyedByInt Int where
  keyedByInt = id

instance (Show h, Show v, KeyedByInt v) => Show (GraphM h v) where
  show g = "GraphT { " ++ show (unHashMap g) ++ " }"

-- | The empty graph
empty :: KeyedByInt v => Graph v
empty = GraphMT HM.empty :: GraphM () v

emptyDGraph :: KeyedByInt v => DirectedGraph v
emptyDGraph = GraphMT HM.empty :: GraphM Directed v

emptyUGraph :: KeyedByInt v => UndirectedGraph v
emptyUGraph = GraphMT HM.empty :: GraphM Undirected v

wrap :: KeyedByInt v => GraphM a v -> v -> Node v
wrap g v = NodeT {key = maximum (HM.keys (unHashMap g)) + 1, unNode = v}

-- | Returns true if a node is an element in the graph
-- isElement :: (Eq h, H.Hashable h) => Graph h v -> GNode h v -> Bool
-- isElement g (GNodeT _ n) = isJust (HM.lookup (hashKey n) (unHashMap g))

-- | Determines the total number of elements in a graph
order :: KeyedByInt v => GraphM a v -> Int
order = HM.size . unHashMap

-- | Insert an element into a graph (keeping the old value if present)
-- insert :: (Eq h, H.Hashable h, Ord h, Num h) => Graph h v -> v -> (GNode h v, Graph h v)
-- insert g v =
--   let nextId = HM.foldl' (\x y -> maximum [x, hashKey y]) 0 (unHashMap g)
--       cgraph = GraphT (HM.union (HM.singleton nextId (NodeT nextId v HM.empty)) (unHashMap g))
--    in (GNodeT cgraph (NodeT nextId v (maybe HM.empty neighbors (HM.lookup nextId (unHashMap cgraph)))), cgraph)
instance (Q.Arbitrary v) => Q.Arbitrary (Node v) where
  arbitrary = do
    int <- H.hash <$> Q.vectorOf 10 (Q.arbitrary :: Q.Gen Char)
    val <- Q.arbitrary :: Q.Gen v
    return (NodeT int val)

-- instance (Q.Arbitrary h, Q.Arbitrary v, H.Hashable h, Eq h) => Q.Arbitrary (Graph h v) where
--   arbitrary = do
--     allNodes <- Q.listOf (Q.arbitrary :: Q.Gen (Node h v)) `Q.suchThat` \x -> length x <= 10
--     let nubbedNodes = L.nubBy (\a b -> hashKey a == hashKey b) (concat [an : HM.elems (neighbors an) | an <- allNodes])
--     return (GraphT (foldl' (\c nxt -> HM.union c (HM.singleton (hashKey nxt) nxt)) HM.empty nubbedNodes))

-- fromDistinctValues :: Eq v => [(v, v)] -> CGraph v
-- fromDistinctValues ls =
--   let hmall = HM.fromList (zip [0 :: Int ..] (nubBy (\a b -> fst a == fst b) ([l | l <- ls] `union` [(vs, []) | vs <- concatMap snd ls])))
--    in --baseHm = HM.mapWithKey (\n v1 -> NodeT n v1 HM.empty) hmneighbors
--       -- fromDistinctValues' acc [] = acc
--       -- fromDistinctValues' acc (c : cs) = fromDistinctValues' (HM.update () HashMap k a)
--       GraphT (HM.mapWithKey (\k1 v1 -> NodeT k1 (fst v1) (HM.singleton k (fst v1))) hmall)
