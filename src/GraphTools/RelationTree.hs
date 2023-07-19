module GraphTools.RelationTree (
  RelationTree,
  relationTree,
) where

import Graphs.BitGraph
import Graphs.PairGraph
import Data.Label
import Graphs.CommonLabeledGraphTypes
import Data.WordTree

type RelationTree = WordTree BitGraph

relationTree :: (LabeledBitGraph, Size) -> RelationTree
relationTree (lbg, s) = wordTreeFromGenerator generator where
  zeroRel = graphOfLabel lbg Zero
  oneRel  = graphOfLabel lbg One
  generator = WordTreeGenerator (diagonal s)
                (\g -> compose s g zeroRel)
                (\g -> compose s g oneRel)
