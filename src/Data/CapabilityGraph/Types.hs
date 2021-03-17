module Data.CapabilityGraph.Types
  ( Graph,
    Node,
    empty,
    order,
    isElement,
    insert,
    wrapNode,
    createEdge,
    adjacent,
    withHashFunction,
    leastCommonAncestor,
    parents,
    allAncestors,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Hashable as H
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Test.QuickCheck as Q

-- | The primary graph structure, most likely will only be interacted w/ wrapped in a more featureful level
newtype Graph v = GraphT
  { -- | underlying data structure, representing the graph as a hashmap from an INT -> a tuple containing a node (value)
    -- | a hashset of ints (w/ the ints being the keys of the nodes parent)
    unHashMap :: HM.HashMap Int (Node v, HS.HashSet Int)
  }

instance Functor Graph where
  f `fmap` (GraphT hm) = GraphT $ HM.map (\(NodeT k n, hs) -> (NodeT k (f n), hs)) hm

-- | the base node object used by the library. Ideally, clients will always interact w/ this wrapped
data Node v = NodeT {key :: Int, unNode :: v}

instance Functor Node where
  f `fmap` n = NodeT {key = key n, unNode = f (unNode n)}

instance (Show v) => Show (Node v) where
  show (NodeT h v) = "NodeT { " ++ show h ++ " " ++ show v ++ " }"

instance (Show v) => Show (Graph v) where
  show g = "GraphT Directed { unHashMap = " ++ show (unHashMap g) ++ " }"

instance Eq v => Eq (Node v) where
  (NodeT k1 v1) == (NodeT k2 v2) = k1 == k2 && v1 == v2

-- | The empty graph
empty :: Graph v
empty = GraphT HM.empty

-- | Returns true if a node is an element of a given graph
isElement :: Graph v -> Node v -> Bool
isElement g n = HM.member (key n) (unHashMap g)

-- | Given a graph and node, update node if already present, otherwise insert, leaving relationships unchanged
insert :: Graph v -> Node v -> Graph v
insert g@(GraphT hm) n@(NodeT k _)
  | HM.member k hm = g
  | otherwise = GraphT (HM.insert k (n, HS.empty) hm)

-- | Create a (directed) edge from the first node to the second one, adding one or both nodes if necessary
createEdge :: Graph v -> Node v -> Node v -> Graph v
createEdge g v1 v2 =
  case (HM.lookup (key v1) (unHashMap g), HM.lookup (key v2) (unHashMap g)) of
    (Just x, Just y) -> GraphT (HM.update (\z -> Just (fst z, (snd z) `HS.union` HS.singleton (key (fst y)))) (key (fst x)) (unHashMap g))
    (Just x, Nothing) -> GraphT (HM.update (\z -> Just (fst x, (snd z) `HS.union` HS.singleton (key v2))) (key (fst x)) (HM.insert (key v2) (v2, HS.empty) (unHashMap g)))
    (Nothing, Just y) -> GraphT (HM.insert (key v1) (v1, HS.singleton (key (fst y))) (unHashMap g))
    (Nothing, Nothing) -> GraphT (HM.insert (key v1) (v1, HS.singleton (key v2)) (HM.insert (key v2) (v2, HS.empty) (unHashMap g)))

-- | Return true if the first node is a child of the second
adjacent :: Graph v -> Node v -> Node v -> Bool
adjacent g v1 v2 = case (HM.lookup (key v1) (unHashMap g), HM.lookup (key v2) (unHashMap g)) of
  (Just x, Just y) -> HS.member (key (fst y)) (snd x)
  _ -> False

-- | Determines the total number of elements in a graph
order :: Graph v -> Int
order = HM.size . unHashMap

-- | Generate a unique hash (w.r.t. a specific graph at an instant) for a value.
wrapNode :: H.Hashable h => (v -> h) -> v -> Node v
wrapNode f v = NodeT (H.hash (f v)) v

-- | Given an adjacency list and a hash function, return a graph containing the same information
withHashFunction :: H.Hashable h => (v -> h) -> [(v, [v])] -> Graph v
withHashFunction f ls =
  let withHashFunction' _ [] acc = acc
      withHashFunction' h (c : cs) acc = case c of
        (x, []) -> withHashFunction' h cs (insert acc (wrapNode h x))
        (x, y : ys) -> withHashFunction' h ((x, ys) : cs) (createEdge acc (wrapNode h x) (wrapNode h y))
   in withHashFunction' f ls empty

-- | given two nodes, returns the node that's reachable in the fewest steps (or Nothing if the nodes are not related)
leastCommonAncestor :: Eq v => Graph v -> Node v -> Node v -> Maybe (Node v)
leastCommonAncestor g n1 n2 =
  case L.intersectBy (\x1 x2 -> snd x1 == snd x2) (allAncestors 0 g n1) (allAncestors 1 g n2) of
    [] -> Nothing
    as -> Just . snd . head $ L.sortOn fst as

-- | return a list of pairs representing all the reachable ancestors of a node and the number of steps taken to get there
allAncestors :: Int -> Graph v -> Node v -> [(Int, Node v)]
allAncestors acc g n = (acc, n) : (parents g n >>= \p -> allAncestors (acc + 1) g p)

-- | return a list of all direct parents of a given node
parents :: Graph v -> Node v -> [Node v]
parents g (NodeT k1 _) = fromMaybe [] (sequence [fst <$> HM.lookup nxt (unHashMap g) | nxt <- HS.toList (HM.findWithDefault HS.empty k1 (HM.map snd (unHashMap g)))])

instance (Q.Arbitrary v) => Q.Arbitrary (Node v) where
  arbitrary = do
    int <- H.hash <$> Q.vectorOf 10 (Q.arbitrary :: Q.Gen Char)
    val <- Q.arbitrary :: Q.Gen v
    return (NodeT int val)

instance (Q.Arbitrary v, H.Hashable v, Eq v) => Q.Arbitrary (Graph v) where
  arbitrary = do
    ns <- Q.listOf (Q.arbitrary :: Q.Gen v)
    let zipped = zip (L.nub [x | x <- ns]) (take (length (L.nub ns)) (L.subsequences [x | x <- ns]))
    return (withHashFunction H.hash zipped)
