module Plan (

) where

import LabeledGraph
import LiftedGraph
import WordTree
import qualified Data.Set as S

{-
 We have a WordTree that maps addresses of Covering nodes to a pointed set. The
point denotes the center of the spiral at the turningWord of the Covering node
and the remaining elements in the set are the required nodes for the covering.

-}

data PointedSet x = PointedSet {
  point :: x,
  set   :: S.Set x
}

type Plan x = WordTree (Maybe (PointedSet x))

executePlan :: Ord x => LabeledGraphI g x -> g -> Plan x -> LiftedGraph x
executePlan gi g plan = result where

