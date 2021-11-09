module SmartSearch (
  Result(..),
  searchUpTo,
) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import ArcCons
import ConciseGraph
import ConciseSubGraph
import CaleyGraph
import DeBruijn
import Graph
import Homo
import DeterminismProperty

import Debug.Trace

data Result = NoHomo | HomoAt Int | UnknownAt Int
  deriving (Eq, Show)

instance Semigroup Result where
  NoHomo <> x    = x
  HomoAt n <> NoHomo = HomoAt n
  HomoAt n <> HomoAt m = HomoAt (min n m)
  HomoAt n <> UnknownAt _ = HomoAt n
  UnknownAt d <> NoHomo = UnknownAt d
  UnknownAt _ <> HomoAt n = HomoAt n
  UnknownAt d <> UnknownAt e = UnknownAt (min d e)

instance Monoid Result where
  mempty = NoHomo

homoAtLevel :: Size -> Int -> (ConciseSubGraph,CaleyGraph) -> Bool
homoAtLevel size level (subgraph,cg) = let
    dim = level
    dom = subdomain subgraph
    deBruijnGraph = dbg dim
    approx = Map.fromSet f (domain dbgI deBruijnGraph)
    f dbgnode = Set.filter (isReallyPossibleValue size cg (nodeToList dim dbgnode) dom) (Set.fromList dom)
  in isPossible approx && not (noHomo (arcConsHomosFromApprox dbgI (conciseSubGraphI size) approx) deBruijnGraph subgraph)

searchLevels :: Size -> [(ConciseSubGraph,CaleyGraph)] -> Int -> Int -> Result
searchLevels size candidates cutoff level =
  if level > cutoff
    then UnknownAt cutoff
    else let 
         in if any (homoAtLevel size level) candidates
              then HomoAt level
              else searchLevels size candidates cutoff (level + 1)

searchUpTo :: Size -> Int -> ConciseGraph -> Result
searchUpTo size cutoff graph = let
    allNodes = Set.fromList (nodes size)
    subsets = Set.filter (\s -> Set.size s >= 2) $ Set.powerSet allNodes
    subgraphs = map (fromSubset size graph) (Set.toList subsets)
    candidates = filter (\s -> not (isWeaklyConstructionDeterministic (conciseSubGraphI size) s)) subgraphs
    candidatesWithCaley = map (\sg -> (sg, ConciseSubGraph.caleyGraph size sg)) candidates
    isReallyGoodPair (sg, cg) = isReallyGoodForDom size cg (subdomain sg)
    reallyGoodCandidates = filter isReallyGoodPair candidatesWithCaley
  in searchLevels size reallyGoodCandidates cutoff 1
