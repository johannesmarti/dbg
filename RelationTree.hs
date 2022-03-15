module RelationTree (
  RelationTree,
  relationTree,
  pathTreesOfCycles,
  firstArcsOnCycles,
  arcsOnCycles,
) where

import Control.Exception.Base
import qualified Data.Set as Set

import BitGraph
import PairGraph
import Graph
import Label
import CommonLGraphTypes
import WordTree

type RelationTree = WordTree BitGraph

relationTree :: (LBitGraph, Size) -> RelationTree
relationTree (lbg, s) = wordTree generator where
  zeroRel = graphOfLabel lbg Zero
  oneRel  = graphOfLabel lbg One
  generator = WordTreeGenerator (diagonal s)
                (\g -> compose s g zeroRel)
                (\g -> compose s g oneRel)

pathTreesOfCycles :: (LBitGraph, Size) -> RelationTree -> [Label] -> [PathTree]
pathTreesOfCycles pair wt word = wts where
  s = snd pair
  wordRel = labelOfWord wt word
  refls = Set.toList $ reflexives (bitGraphI s) wordRel
  cycleTreeOfRefl r = pathTree pair wt word r r
  wts = map cycleTreeOfRefl refls

firstArcsOnCycles :: (LBitGraph, Size) -> RelationTree -> [Label] -> [(Int,Label,Int)]
firstArcsOnCycles pair wt word =
  concatMap firstArcs (pathTreesOfCycles pair wt word)

arcsOnCycles :: (LBitGraph, Size) -> RelationTree -> [Label] -> [(Int,Label,Int)]
arcsOnCycles pair wt word =
  concatMap allArcs (pathTreesOfCycles pair wt word)

-- Doing all of this with sets might be better!
data PathTree = There Int | Step Int Label [PathTree]
  deriving Show

extractNode :: PathTree -> Int
extractNode (There n) = n
extractNode (Step n _ _) = n

firstArcs :: PathTree -> [(Int,Label,Int)]
firstArcs (There _) = []
firstArcs (Step s l succs) = [(s,l,extractNode t) | t <- succs]

allArcs :: PathTree -> [(Int,Label,Int)]
allArcs (There _) = []
allArcs (Step s l succs) = [(s,l,extractNode t) | t <- succs] ++
                              concatMap allArcs succs

pathTree :: (LBitGraph, Size) -> RelationTree -> [Label] -> Int -> Int -> PathTree
pathTree _ _ [] source target = assert (source == target) $ There target
pathTree (lbg,s) rt (next:rest) source target = let
    nextRel = graphOfLabel lbg next
    restRel = labelOfWord rt rest
    succsSource = successors (bitGraphI s) nextRel source
    predsTarget = predecessors (bitGraphI s) restRel target
    nextNodes = Set.toList $ succsSource `Set.intersection` predsTarget
    mapper ns = pathTree (lbg,s) rt rest ns target
  in Step source next (map mapper nextNodes)
