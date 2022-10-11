module PathTree (
  PathTree(..),
  extractNode,
  pathTreesOfMCycles,
  firstArcsOnMCycles,
  arcsOnMCycles,
  pathesOnPathTree,
) where

import Control.Exception.Base
import qualified Data.Set as Set

import BitGraph
import CommonLGraphTypes
import Graph
import Label
import PairGraph
import qualified Path


-- Doing all of this with sets might be better!
data PathTree = There Node | Step Node Label [PathTree]
  deriving Show

extractNode :: PathTree -> Node
extractNode (There n) = n
extractNode (Step n _ _) = n

firstArcs :: PathTree -> [(Node,Label,Node)]
firstArcs (There _) = []
firstArcs (Step s l succs) = [(s,l,extractNode t) | t <- succs]

allArcs :: PathTree -> [(Node,Label,Node)]
allArcs (There _) = []
allArcs (Step s l succs) = [(s,l,extractNode t) | t <- succs] ++
                              concatMap allArcs succs

pathTree :: (LBitGraph, Size) -> ([Label] -> BitGraph) -> [Label] -> Node -> Node -> PathTree
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

firstArcsOnMCycles :: (LBitGraph, Size) -> ([Label] -> BitGraph) -> [Label] -> [(Node,Label,Node)]
firstArcsOnMCycles pair relOfWord word =
  concatMap firstArcs (pathTreesOfMCycles pair relOfWord word)

arcsOnMCycles :: (LBitGraph, Size) -> ([Label] -> BitGraph) -> [Label] -> [(Node,Label,Node)]
arcsOnMCycles pair relOfWord word =
  concatMap allArcs (pathTreesOfMCycles pair relOfWord word)

pathesOnPathTree :: PathTree -> [Path.Path Node]
pathesOnPathTree (There x) = [Path.There x]
pathesOnPathTree (Step x l cs) =
  map prepender $ concatMap pathesOnPathTree cs where
    prepender path = Path.Step x l path

