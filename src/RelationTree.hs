module RelationTree (
  RelationTree,
  relationTree,
) where

import BitGraph
import PairGraph
import Data.Label
import CommonLabeledGraphTypes
import Data.WordTree

type RelationTree = WordTree BitGraph

relationTree :: (LabeledBitGraph, Size) -> RelationTree
relationTree (lbg, s) = wordTreeFromGenerator generator where
  zeroRel = graphOfLabel lbg Zero
  oneRel  = graphOfLabel lbg One
  generator = WordTreeGenerator (diagonal s)
                (\g -> compose s g zeroRel)
                (\g -> compose s g oneRel)
