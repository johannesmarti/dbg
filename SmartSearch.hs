module SmartSearch (
  Result(..),
  searchUpTo,
  subPathCondition,
) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import ArcCons
import Bitify
import CommonLGraphTypes
import BitGraph (Node,Size)
import LWrappedGraph
import CayleyGraph hiding (domain)
import DeBruijnGraph
import LabeledGraph
import Homo
import DeterminismProperty
import Coding hiding (domain)

data Result = NoHomo | HomoAt Int | UnknownAt Int
  deriving (Eq, Show)

{-
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
-}

homoAtLevel :: Ord x => Int -> (LMapGraph x,LWrappedGraph LBitGraph Node x,Size,CayleyGraph) -> Bool
homoAtLevel level (g,wg,s,cg) = let
    dim = level
    deBruijnGraph = dbg dim
    c = coding wg
    dom = domain lMapGraphINotPretty g
    approx = Map.fromSet f (domain dbgI deBruijnGraph)
    f dbgnode = Set.filter (isPossVal (nodeToList dim dbgnode)) dom
    isPossVal word node = isPossibleValue s cg word (encode c node)
  in isPossible approx && (not (noHomo (arcConsHomosFromApprox dbgI lMapGraphINotPretty approx) deBruijnGraph g))

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
  in isPossible approx && (not (noHomo (arcConsHomosFromApprox dbgI bgI approx) deBruijnGraph bg))
-}

searchLevels :: Ord x => [(LMapGraph x,LWrappedGraph LBitGraph Node x,Size,CayleyGraph)] -> Int -> Int -> Result
searchLevels candidates cutoff level =
  if level > cutoff
    then UnknownAt cutoff
    else let 
         in if any (homoAtLevel level) candidates
              then HomoAt level
              else searchLevels candidates cutoff (level + 1)

searchUpTo :: Ord x => Int -> LabeledGraphI g x -> g -> Result
searchUpTo cutoff gi graph = let
    dom = domain gi graph
    subsets = Set.toList $ Set.filter (\s -> Set.size s >= 2) $ Set.powerSet dom
    subgraphs = map (lMapSubgraphFromLGraph gi graph) subsets
    candidates = filter (\s -> not (isConstructionDeterministic lMapGraphINotPretty s)) subgraphs
    bityCandidates = map (\c -> (c, labeledBitify lMapGraphINotPretty c)) candidates
    candidatesWithCayley = map (\(g,(wg,s)) -> (g, wg, s, cayleyGraphOfLBitGraph s(innerGraph wg))) bityCandidates
    isGoodCandi (g, wg, s, cg) = pathCondition s cg
    goodCandidates = filter isGoodCandi candidatesWithCayley
  in if null goodCandidates
       then NoHomo
       else searchLevels goodCandidates cutoff 1

subPathCondition :: Ord x => LabeledGraphI g x -> g -> Bool
subPathCondition gi graph = hasT1 gi graph || let
    dom = domain gi graph
    subsets = Set.toList $ Set.filter (\s -> Set.size s >= 3) $ Set.powerSet dom
    subgraphs = map (lMapSubgraphFromLGraph gi graph) subsets
    bityCandidates = map (\c -> labeledBitify lMapGraphINotPretty c) subgraphs
    cayleys = map (\(wg,s) -> (cayleyGraphOfLBitGraph s (innerGraph wg), s)) bityCandidates
   in any (\(cg, s) -> pathCondition s cg) cayleys
