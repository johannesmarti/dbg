module RelationTree (

) where

import Label
import LiftedGraph
import WordTree

relationTree :: LiftedGraph x -> WordTree BitGraph
relationTree lg = wordTree generator where
  (lbg,s) = toLBitGraph lg
  zeroRel = graphOfLabel lbg Zero
  oneRel  = graphOfLabel lbg One
  generator = WordTreeGenerator (diagonal s)
                (\g -> compose s g zeroRel)
                (\g -> compose s g oneRel)

allArcsOnLoops :: WordTree BitGraph -> [Label] -> [(Int,Label,Int)]
allArcsOnLoops = undefined

data PathTree = There | Step Int Label [PathTree]

pathTree :: WordTree BitGraph -> Word -> Int -> Int -> PathTree
pathTree rt [] source target = assert (source == target) $ There
pathTree rt (next:rest) source target = undefined
