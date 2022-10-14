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

import RelationCache
import Graph
import Label
import qualified Path


-- Doing all of this with sets might be better!
data PathTree x = There x | Step x Label [PathTree x]
  deriving Show

extractNode :: PathTree x -> x
extractNode (There n) = n
extractNode (Step n _ _) = n

firstArcs :: PathTree x -> [(x,Label,x)]
firstArcs (There _) = []
firstArcs (Step s l succs) = [(s,l,extractNode t) | t <- succs]

allArcs :: PathTree x -> [(x,Label,x)]
allArcs (There _) = []
allArcs (Step s l succs) = [(s,l,extractNode t) | t <- succs] ++
                              concatMap allArcs succs

pathTree :: Ord x => RelationCache r x -> [Label] -> x -> x -> PathTree x
pathTree _ [] source target = assert (source == target) $ There target
pathTree cache (next:rest) source target = let
    relI = outputType cache
    relOfWord = relationOfWord cache
    nextRel = relOfWord [next]
    restRel = relOfWord rest
    succsSource = successors relI nextRel source
    predsTarget = predecessors relI restRel target
    nextNodes = Set.toList $ succsSource `Set.intersection` predsTarget
    mapper ns = pathTree cache rest ns target
  in Step source next (map mapper nextNodes)

pathTreesOfMCycles :: Ord x => RelationCache r x -> [Label] -> [PathTree x]
pathTreesOfMCycles cache word = relOfWords where
  relOfWord = relationOfWord cache
  wordRel = relOfWord word
  refls = Set.toList $ reflexivesUniversalInMultiple cache wordRel
  cycleTreeOfRefl r = pathTree cache word r r
  relOfWords = map cycleTreeOfRefl refls

firstArcsOnMCycles :: Ord x => RelationCache r x -> [Label] -> [(x,Label,x)]
firstArcsOnMCycles cache word =
  concatMap firstArcs (pathTreesOfMCycles cache word)

arcsOnMCycles :: Ord x => RelationCache r x -> [Label] -> [(x,Label,x)]
arcsOnMCycles cache word =
  concatMap allArcs (pathTreesOfMCycles cache word)

pathesOnPathTree :: PathTree x -> [Path.Path x]
pathesOnPathTree (There x) = [Path.There x]
pathesOnPathTree (Step x l cs) =
  map prepender $ concatMap pathesOnPathTree cs where
    prepender path = Path.Step x l path
