module Plan (
  Spoke,
  spoke,
  Plan,
  Plan.empty,
  Plan.insert,
) where

-- TODO: Should we use the strict or the lazy state monad? Need to read up!
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

import CoveringGraph
import LabeledGraph
import LiftedGraph
import WordTree

import WordMap.Algebraic as WordMap

{-
 We have a WordTree that maps addresses of Covering nodes to a pointed set. The
point denotes the center of the spiral at the turningWord of the Covering node
and the remaining elements in the set are the required nodes for the covering.

-}

data Spoke x = Spoke {
  hub     :: x,
  points  :: Map.Map x Int
}

spoke :: Ord x => x -> [(x,Int)] -> Spoke x
spoke h p = Spoke h (Map.insert h 0 (Map.fromList p))

type Plan x = WordMap (Spoke x)

empty :: Plan x
empty = WordMap.empty

insert :: [Label] -> Spoke x -> Plan x -> Plan x
insert = WordMap.insert

executePlan :: Ord x => LabeledGraphI g x -> g -> Plan x -> LiftedGraph x
executePlan gi g plan = result where
  result = undefined

{-
wrap-up/roll a cycle:
- Check that the ancestor of all nodes on the cycle have been completely wrapped. Otherwise do it.
- Wrap up the nodes on the spiral for the cycle
-- Count up d = 1 .. n and wrap up all nodes with distance d (Once nodes for d hhave been wrapped we should be able to wrap nodes of dinstance d + 1) At each stage keep track of the node at the hub.
-- Check that all the children of the nodes on the hub have been constructed and return them in a reasonable order.
-- wrap these children in a raesonable order.
-- store the fat hub nodes in the partial result word map
-}

data PartialResult x = PartialResult {
  liftedGraph :: LiftedGraph x,
  constructedNodes :: WordMap Int
}

constructNode :: Plan x -> CoveringNode -> State (PartialResult x) int
constructNode = undefined

