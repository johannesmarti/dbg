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

data Result = NoHomo | HomoAt Int | Unknown
  deriving (Eq, Show)

instance Semigroup Result where
  NoHomo <> x    = x
  HomoAt n <> _  = HomoAt n
  Unknown <> NoHomo = Unknown
  Unknown <> HomoAt n = HomoAt n
  Unknown <> Unknown = Unknown

instance Monoid Result where
  mempty = NoHomo

searchDbgHomo :: Size -> Int -> ConciseSubGraph -> Result
searchDbgHomo size cutoff subgraph = let
    cg = ConciseSubGraph.caleyGraph size subgraph
    dom = subdomain subgraph
    searchHomo dim = if dim > cutoff then Unknown
        else let 
                deBruijnGraph = dbg dim
                approx = Map.fromSet f (domain dbgI deBruijnGraph)
                f dbgnode = Set.filter (isReallyPossibleValue size cg (nodeToList dim dbgnode) dom) (Set.fromList dom)
             in if isPossible approx && not (noHomo (arcConsHomosFromApprox dbgI (conciseSubGraphI size) approx) deBruijnGraph subgraph)
                  then HomoAt dim
                  else searchHomo (dim + 1)
  in if isReallyGoodForDom size cg dom
       then searchHomo 1
       else NoHomo

searchUpTo :: Size -> Int -> ConciseGraph -> Result
searchUpTo size cutoff graph = let
    allNodes = Set.fromList (nodes size)
    subsets = Set.filter (\s -> Set.size s >= 2) $ Set.powerSet allNodes
    subgraphs = map (fromSubset size graph) (Set.toList subsets)
    candidates = filter (\s -> not (isWeaklyConstructionDeterministic (conciseSubGraphI size) s)) subgraphs
  in mconcat (map (searchDbgHomo size cutoff) candidates)
