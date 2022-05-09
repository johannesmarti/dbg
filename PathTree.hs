module PathTree (
  PathTree(..),
  extractNode,
  pathTreesOfMCycles,
  firstArcsOnMCycles,
  arcsOnMCycles,
) where

import Control.Exception.Base
import qualified Data.Set as Set

import BitGraph
import CommonLGraphTypes
import Graph
import Label
import PairGraph


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

pathTree :: (LBitGraph, Size) -> ([Label] -> BitGraph) -> [Label] -> Int -> Int -> PathTree
pathTree _ _ [] source target = assert (source == target) $ There target
pathTree (lbg,s) relOfWord (next:rest) source target = let
    nextRel = graphOfLabel lbg next
    restRel = relOfWord rest
    succsSource = successors (bitGraphI s) nextRel source
    predsTarget = predecessors (bitGraphI s) restRel target
    nextNodes = Set.toList $ succsSource `Set.intersection` predsTarget
    mapper ns = pathTree (lbg,s) relOfWord rest ns target
  in Step source next (map mapper nextNodes)

pathTreesOfMCycles :: (LBitGraph, Size) -> ([Label] -> BitGraph) -> [Label] -> [PathTree]
pathTreesOfMCycles pair relOfWord word = relOfWords where
  s = snd pair
  wordRel = relOfWord word
  refls = Set.toList $ reflexivesUnivInMultiple s wordRel
  cycleTreeOfRefl r = pathTree pair relOfWord word r r
  relOfWords = map cycleTreeOfRefl refls

firstArcsOnMCycles :: (LBitGraph, Size) -> ([Label] -> BitGraph) -> [Label] -> [(Int,Label,Int)]
firstArcsOnMCycles pair relOfWord word =
  concatMap firstArcs (pathTreesOfMCycles pair relOfWord word)

arcsOnMCycles :: (LBitGraph, Size) -> ([Label] -> BitGraph) -> [Label] -> [(Int,Label,Int)]
arcsOnMCycles pair relOfWord word =
  concatMap allArcs (pathTreesOfMCycles pair relOfWord word)

