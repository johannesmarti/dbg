module SmartSearch (
  Result(..),
  searchUpTo,
  subPathCondition,
) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import AllocateWords
import ArcCons
import Bitable
import CommonLGraphTypes
import BitGraph (Node,Size,nodesSet)
import LWrappedGraph
import RelationCache
import CayleyGraph hiding (domain)
import DeBruijnGraph
import LabeledGraph
import Homomorphism
import DeterminismProperty
import Coding hiding (domain)

data Result = NoHomomorphism | HomomorphismAt Int | UnknownAt Int
  deriving (Eq, Show)

homoAtLevel :: Ord x => Int -> (LMapGraph x, Bitification x, CayleyGraph)
                        -> Bool
homoAtLevel level (g,bf,cg) = let
    dim = level
    deBruijnGraph = dbg dim
    lbg = labeledBitGraph bf
    s = numBits bf
    dom = nodesSet s
    approx = Map.fromSet f (domain dbgI deBruijnGraph)
    f dbgnode = Set.filter (isPossVal (nodeToList dim dbgnode)) dom
    relOfWord = relationOfWord (relationCache bf cg)
    isPossVal word node = isPossibleValue s relOfWord word node
  in isPossible approx && (not (noHomomorphism (arcConsHomomorphismsFromApprox dbgI (lBitGraphI s) approx) deBruijnGraph lbg))

{-
homoAtLevel :: (Ord x, Pretty x) => Int -> (LMapGraph x,LWrappedGraph LBitGraph Node x,Size,CayleyGraph) -> Bool
homoAtLevel level (g,wg,s,cg) = let
    dim = level
    deBruijnGraph = dbg dim
    bg = innerGraph wg
    bgI = lBitGraphI s
    dom = domain bgI bg
    approx = Map.fromSet f (domain dbgI deBruijnGraph)
    f dbgnode = Set.filter (isPossibleValue s cg (nodeToList dim dbgnode)) dom
  in isPossible approx && (not (noHomomorphism (arcConsHomomorphismsFromApprox dbgI bgI approx) deBruijnGraph bg))
-}

searchLevels :: Ord x => [(LMapGraph x, Bitification x, CayleyGraph)]
                         -> Int -> Int -> Result
searchLevels candidates cutoff level =
  if level > cutoff
    then UnknownAt cutoff
    else let 
         in if any (homoAtLevel level) candidates
              then HomomorphismAt level
              else searchLevels candidates cutoff (level + 1)

searchUpTo :: Ord x => Int -> LabeledGraphI g x -> g -> Result
searchUpTo cutoff gi graph = let
    dom = domain gi graph
    subsets = Set.toList $ Set.filter (\s -> Set.size s >= 2) $ Set.powerSet dom
    subgraphs = map (lMapSubgraphFromLGraph gi graph) subsets
    candidates = filter (\s -> not (isConstructionDeterministic lMapGraphINotPretty s)) subgraphs
    bityCandidates = map (\c -> (c, genericBitableI lMapGraphINotPretty c)) candidates
    candidatesWithCayley = map (\(g,bitification) -> (g, bitification, rightCayleyGraph bitification)) bityCandidates
    isGoodCandi (g, bf, cg) = pathCondition (numBits bf) cg
    goodCandidates = filter isGoodCandi candidatesWithCayley
  in if null goodCandidates
       then NoHomomorphism
       else searchLevels goodCandidates cutoff 1

subPathCondition :: Ord x => LabeledGraphI g x -> g -> Bool
subPathCondition gi graph = hasT1 gi graph || let
    dom = domain gi graph
    subsets = Set.toList $ Set.filter (\s -> Set.size s >= 3) $ Set.powerSet dom
    subgraphs = map (lMapSubgraphFromLGraph gi graph) subsets
    bityCandidates = map (\c -> genericBitableI lMapGraphINotPretty c) subgraphs
    cayleys = map (\bf -> (rightCayleyGraph bf, numBits bf)) bityCandidates
   in any (\(cg, s) -> pathCondition s cg) cayleys
