module Plan (

) where

import LabeledGraph
import LiftedGraph
import WordTree
import qualified Data.Map.Strict as M

{-
 We have a WordTree that maps addresses of Covering nodes to a pointed set. The
point denotes the center of the spiral at the turningWord of the Covering node
and the remaining elements in the set are the required nodes for the covering.

-}

data Spoke x = Spoke {
  hub     :: x,
  spoints :: M.Map x Int
}

type Plan x = WordTree (Maybe (Spoke x))

executePlan :: Ord x => LabeledGraphI g x -> g -> Plan x -> LiftedGraph x
executePlan gi g plan = result where
  result = undefined

