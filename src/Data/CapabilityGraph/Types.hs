module Data.CapabilityGraph.Types
  ( Graph,
    Node,
    empty,
    order,
    isElement,
    insert,
    fromKeyed,
    wrapContext,
    createEdge,
    adjacent,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Hashable as H
import qualified Test.QuickCheck as Q

-- | The primary graph structure, most likely will only be interacted w/ wrapped in a more featureful level
newtype Graph v = GraphT
  { -- | underlying data structure, representing the graph as a hashmap from an INT -> a tuple containing a node (value)
    -- | a hashset of ints (w/ the ints being the keys of the nodes parent)
    unHashMap :: HM.HashMap Int (Node v, HS.HashSet Int)
  }

instance Functor Graph where
  f `fmap` (GraphT hm) = GraphT $ HM.map (\(NodeT k n, hs) -> (NodeT k (f n), hs)) hm

data Node v = NodeT {key :: Int, unNode :: v}

instance Functor Node where
  f `fmap` n = NodeT {key = key n, unNode = f (unNode n)}

instance (Show v) => Show (Node v) where
  show (NodeT h v) = "NodeT { " ++ show h ++ " " ++ show v ++ " }"

instance (Show v) => Show (Graph v) where
  show g = "GraphT Directed { unHashMap = " ++ show (unHashMap g) ++ " }"

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

createEdge :: Graph v -> Node v -> Node v -> Graph v
createEdge g v1 v2 =
  case (HM.lookup (key v1) (unHashMap g), HM.lookup (key v2) (unHashMap g)) of
    (Just x, Just y) -> GraphT (HM.update (\z -> Just (fst z, (snd z) <> HS.singleton (key (fst y)))) (key (fst x)) (unHashMap g))
    (Just x, Nothing) -> GraphT (HM.update (\z -> Just (fst x, (snd z) <> HS.singleton (key v1))) (key (fst x)) (unHashMap g))
    (Nothing, Just y) -> GraphT (HM.insert (key v1) (v1, HS.singleton (key (fst y))) (unHashMap g))
    (Nothing, Nothing) -> GraphT ((HM.insert (key v2) (v2, HS.empty)) (unHashMap g) <> HM.insert (key v1) (v2, HS.singleton (key v1)) (unHashMap g))

adjacent :: Graph v -> Node v -> Node v -> Bool
adjacent g v1 v2 = case (HM.lookup (key v1) (unHashMap g), HM.lookup (key v2) (unHashMap g)) of
  (Just x, Just y) -> HS.member (key (fst y)) (snd x)
  _ -> False

-- | Determines the total number of elements in a graph
order :: Graph v -> Int
order = HM.size . unHashMap

-- | Generate a unique hash (w.r.t. a specific graph at an instant) for a value.
wrapContext :: Graph v -> v -> Node v
wrapContext graph v =
  let currHashes = map (key . fst) (HM.elems (unHashMap graph))
      nextHash = case currHashes of
        [] -> 1
        hs -> 1 + maximum hs
   in NodeT nextHash v

-- Given a list of tuples representing a mapping from value -> parent values (w/ the ints the values' keys), return a graph containg the same information
fromKeyed :: [((Int, v), [(Int, v)])] -> Graph v
fromKeyed keyed = GraphT (HM.fromList $ [(fst k1, (NodeT (fst k1) (snd k1), HS.fromList (map fst v))) | (k1, v) <- keyed] ++ [(fst k1, (NodeT (fst k1) (snd k1), HS.empty)) | k1 <- concatMap snd keyed])

instance (Q.Arbitrary v) => Q.Arbitrary (Node v) where
  arbitrary = do
    int <- H.hash <$> Q.vectorOf 10 (Q.arbitrary :: Q.Gen Char)
    val <- Q.arbitrary :: Q.Gen v
    return (NodeT int val)
