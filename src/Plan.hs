module Plan (
  Spoke,
  spoke,
  Plan,
  Plan.empty,
  Plan.insert,
) where

import LabeledGraph
import LiftedGraph
import WordTree
import qualified Data.Map.Strict as Map

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

