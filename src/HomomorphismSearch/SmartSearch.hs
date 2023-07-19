module HomomorphismSearch.SmartSearch (
  Result(..),
  searchUpTo,
  subPathCondition,
) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Graphs.LabeledGraphInterface
import Graphs.CommonLabeledGraphTypes
import Graphs.BitGraph (Node,Size,nodesSet)
import Graphs.DeBruijnGraph
import Bitify.Bitifier
import Bitify.Coding hiding (domain)
import GraphTools.RelationCache
import Conditions.CayleyGraph hiding (domain, relationOfWord)
import Conditions.Constructible
import HomomorphismSearch.AllocateWords
import HomomorphismSearch.ArcCons
import HomomorphismSearch.Homomorphism

data Result = NoHomomorphism | HomomorphismAt Int | UnknownAt Int
  deriving (Eq, Show)

homoAtLevel :: Ord x => Int -> (Bitification x, CayleyGraph) -> Bool
homoAtLevel level (bf,cg) = let
    dim = level
    deBruijnGraph = dbg dim
    lbg = labeledBitGraph bf
    s = numBits bf
    dom = nodesSet s
    approx = Map.fromSet f (domain dbgInterface deBruijnGraph)
    f dbgnode = Set.filter (isPossVal (nodeToList dim dbgnode)) dom
    relationCache = relationCacher bf cg
    relOfWord = relationOfWord relationCache
    isPossVal word node = isPossibleValue s relOfWord word node
  in isPossible approx && (not (noHomomorphism (arcConsHomomorphismsFromApprox dbgInterface (labeledBitGraphInterface s) approx) deBruijnGraph lbg))

{-
homoAtLevel :: (Ord x, Pretty x) => Int -> (LabeledMapGraph x,LWrappedGraph LBitGraph Node x,Size,CayleyGraph) -> Bool
homoAtLevel level (g,wg,s,cg) = let
    dim = level
    deBruijnGraph = dbg dim
    bg = innerGraph wg
    bgInterface = labeledBitGraphInterface s
    dom = domain bgInterface bg
    approx = Map.fromSet f (domain dbgInterface deBruijnGraph)
    f dbgnode = Set.filter (isPossibleValue s cg (nodeToList dim dbgnode)) dom
  in isPossible approx && (not (noHomomorphism (arcConsHomomorphismsFromApprox dbgInterface bgInterface approx) deBruijnGraph bg))
-}

searchLevels :: Ord x => [(Bitification x, CayleyGraph)] -> Int -> Int -> Result
searchLevels candidates cutoff level =
  if level > cutoff
    then UnknownAt cutoff
    else let 
         in if any (homoAtLevel level) candidates
              then HomomorphismAt level
              else searchLevels candidates cutoff (level + 1)

searchUpTo :: Ord x => Int -> LabeledGraphInterface g x -> g -> Result
searchUpTo cutoff gi graph = let
    dom = domain gi graph
    subsets = Set.toList $ Set.filter (\s -> Set.size s >= 2) $ Set.powerSet dom
    subgraphs = map (labeledMapSubgraphFromLabeledGraph gi graph) subsets
    candidates = filter (\s -> isConstructible labeledMapGraphInterfaceNotPretty s) subgraphs
    bityCandidates = map (\c -> genericBitifier labeledMapGraphInterfaceNotPretty c) candidates
    candidatesWithCayley = map (\bitification -> (bitification, rightCayleyGraph bitification)) bityCandidates
    isGoodCandi (bf, cg) = pathCondition (numBits bf) cg
    goodCandidates = filter isGoodCandi candidatesWithCayley
  in if null goodCandidates
       then NoHomomorphism
       else searchLevels goodCandidates cutoff 1

subPathCondition :: Ord x => LabeledGraphInterface g x -> g -> Bool
subPathCondition gi graph = hasT1 gi graph || let
    dom = domain gi graph
    subsets = Set.toList $ Set.filter (\s -> Set.size s >= 3) $ Set.powerSet dom
    subgraphs = map (labeledMapSubgraphFromLabeledGraph gi graph) subsets
    bityCandidates = map (\c -> genericBitifier labeledMapGraphInterfaceNotPretty c) subgraphs
    cayleys = map (\bf -> (rightCayleyGraph bf, numBits bf)) bityCandidates
   in any (\(cg, s) -> pathCondition s cg) cayleys
