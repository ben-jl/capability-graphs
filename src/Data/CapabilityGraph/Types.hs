{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.CapabilityGraph.Types
  ( Graph,
    DirectedGraph,
    UndirectedGraph,
    Node,
    emptyDGraph,
    emptyUGraph,
    empty,
    order,
    isElement,
    upsert,
    nodes,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Hashable as H
import Data.Maybe (fromMaybe)
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
    unHashMap :: HM.HashMap Int (Node v, HS.HashSet Int)
  }

instance Functor (GraphM a) where
  f `fmap` g = GraphMT (HM.map (\(NodeT k v, hs) -> (NodeT k (f v), hs)) (unHashMap g))

instance (Show h, Show v) => Show (GraphM h v) where
  show g = "GraphT { " ++ show (unHashMap g) ++ " }"

-- | The empty graph
empty :: Graph v
empty = GraphMT HM.empty :: GraphM () v

emptyDGraph :: DirectedGraph v
emptyDGraph = GraphMT HM.empty :: GraphM Directed v

emptyUGraph :: UndirectedGraph v
emptyUGraph = GraphMT HM.empty :: GraphM Undirected v

wrap :: GraphM a v -> v -> Node v
wrap g v = NodeT {key = maximum (HM.keys (unHashMap g)) + 1, unNode = v}

-- | Returns true if a node is an element of a given graph
isElement :: GraphM a v -> Node v -> Bool
isElement g n = HM.member (key n) (unHashMap g)

-- | Given a graph and node, update node if already present, otherwise insert, leaving relationships unchanged
upsert :: GraphM a v -> Node v -> GraphM a v
upsert g n =
  let currhs = maybe HS.empty snd (HM.lookup (key n) (unHashMap g))
   in GraphMT (HM.insert (key n) (n, currhs) (unHashMap g))

-- | Determines the total number of elements in a graph
order :: GraphM a v -> Int
order = HM.size . unHashMap

-- | return a list of all nodes in the given graph
nodes :: GraphM a v -> [Node v]
nodes g = fst <$> HM.elems (unHashMap g)

-- | Returns true if a keyable object
instance (Q.Arbitrary v) => Q.Arbitrary (Node v) where
  arbitrary = do
    int <- H.hash <$> Q.vectorOf 10 (Q.arbitrary :: Q.Gen Char)
    val <- Q.arbitrary :: Q.Gen v
    return (NodeT int val)

instance (Q.Arbitrary v) => Q.Arbitrary (GraphM a v) where
  arbitrary = do
    mnodes <- Q.listOf (Q.arbitrary :: Q.Gen (Node v))
    relatives <- Q.vectorOf (length mnodes) (Q.listOf (Q.elements mnodes))
    let zipped = zip mnodes relatives
    let res = map (\(b, ps) -> HM.singleton (key b) (b, mconcat [HS.singleton (key p) | p <- ps])) zipped
    return (GraphMT (mconcat res))
