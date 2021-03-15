module Data.CapabilityGraph.Types
  ( Graph,
    Node,
    empty,
    order,
    isElement,
    upsert,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Hashable as H
import qualified Test.QuickCheck as Q

--  (HM.HashMap h (Node v, HS.HashSet h))
newtype Graph v = GraphT
  { unHashMap :: HM.HashMap Int (Node v, HS.HashSet Int)
  }

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
upsert :: Graph v -> Node v -> Graph v
upsert g n =
  let currhs = maybe HS.empty snd (HM.lookup (key n) (unHashMap g))
   in GraphT (HM.insert (key n) (n, currhs) (unHashMap g))

-- | Determines the total number of elements in a graph
order :: Graph v -> Int
order = HM.size . unHashMap

instance (Q.Arbitrary v) => Q.Arbitrary (Node v) where
  arbitrary = do
    int <- H.hash <$> Q.vectorOf 10 (Q.arbitrary :: Q.Gen Char)
    val <- Q.arbitrary :: Q.Gen v
    return (NodeT int val)
