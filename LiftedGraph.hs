module LiftedGraph (
  LiftedGraph,
  intGraphI,
  graph,
  fromLGraph,
  combine,
  prettyLiftedGraph,
  LiftingCandidate,
  prettyCandidate,
  extractPair,
  liftableCandidates,
  liftablePairs,
  weakDominationFilter,
) where

import Debug.Trace

import Control.Exception.Base (assert)
import Control.Monad.State.Lazy
import Data.Map.Strict as Map hiding (filter, map)
import qualified Data.Set as Set

import Coding hiding (domain)
import CommonLGraphTypes
import LabeledGraph
import Lifted
import Tools (strictPairs)
import Pretty (stdPrintSet)

data Justification x = Base x | Doubleton Int Int

base :: x -> Justification x
base a = Base a

doubleton' :: Int -> Int -> Justification x
doubleton' a b = assert (a < b) $ Doubleton a b

doubleton :: Int -> Int -> Justification x
doubleton a b = case compare a b of
  LT -> doubleton' a b
  EQ -> error "Can not create doubleton justification of two equal numbers"
  GT -> doubleton' b a

type IntGraph = LMapGraph Int

intGraphI :: LabeledGraphI IntGraph Int
intGraphI = lMapGraphI

data LiftedGraph x = LiftedGraph {
  graph         :: IntGraph,
  justification :: Map Int (Justification x),
  printBase     :: x -> String
}

allJustified :: LiftedGraph x -> Bool
allJustified lg = keysSet (justification lg) == domain intGraphI (graph lg)

justify :: LiftedGraph x -> Int -> Justification x
justify lg node = assert (allJustified lg) $
  case Map.lookup node (justification lg) of
    Just j  -> j
    Nothing -> error "node is not in LiftedGraph"

topNode :: LiftedGraph x -> Int
topNode lg = case Set.lookupMax (domain intGraphI (graph lg)) of
               Just m  ->  m
               Nothing -> -1

nextNode :: LiftedGraph x -> Int
nextNode lg = topNode lg + 1

fromLGraph :: Ord x => LabeledGraphI g x -> g -> LiftedGraph x
fromLGraph gi g = LiftedGraph intGraph just pb where
  coding = codeSet (domain gi g)
  intGraph = lMapApplyBijection gi g (encode coding)
  just = fromSet justifyBase (domain intGraphI intGraph)
  justifyBase i = base (decode coding i)
  pb = prettyNode gi g

type LiftingCandidate = ((Set.Set Int, Set.Set Int), (Int,Int), (Set.Set Int, Set.Set Int))

prettyCandidate :: LiftingCandidate -> String
prettyCandidate ((zp,op),(u,v),(zs,os)) =
    stdPrintSet show zp ++ " 0> " ++ pair ++ "  0> " ++ stdPrintSet show zs ++ "\n"
      ++ stdPrintSet show op ++ " 1> " ++ pair ++ "  1> " ++ stdPrintSet show os ++ "\n" where
        pair = "[" ++ show u ++ " " ++ show v ++ "]"

extractPair :: LiftingCandidate -> (Int,Int)
extractPair (pre,pair,suc) = pair

extractPreds :: Label -> LiftingCandidate -> Set.Set Int
extractPreds Zero (pre,_,_) = fst pre
extractPreds One  (pre,_,_) = snd pre

extractSuccs :: Label -> LiftingCandidate -> Set.Set Int
extractSuccs Zero (_,_,suc) = fst suc
extractSuccs One  (_,_,suc) = snd suc 

computeCandidate :: IntGraph -> (Int,Int) -> LiftingCandidate
computeCandidate g (a,b) =
  ((pred Zero a `Set.intersection` pred Zero b,
    pred One a `Set.intersection` pred One b),
   (a,b),
   (succ Zero a `Set.union` succ Zero b,
    succ One a `Set.union` succ One b)) where
      succ = successors intGraphI g
      pred = predecessors intGraphI g

isVisible :: LiftingCandidate -> Bool
isVisible can = not (Set.null $ extractPreds Zero can) && not (Set.null $ extractPreds One can)

liftableCandidates :: IntGraph -> [LiftingCandidate]
liftableCandidates g = let
    domList = Set.toList $ domain intGraphI g
    alldoubles = strictPairs domList
    candidates = filter isVisible (map (computeCandidate g) alldoubles)
  in candidates

liftablePairs :: IntGraph -> [(Int,Int)]
liftablePairs g = map extractPair $ liftableCandidates g

liftCandidate :: LiftingCandidate -> State (LiftedGraph x) Int
liftCandidate can = state $ \lg ->
  assert (can == computeCandidate (graph lg) (extractPair can)) $
  assert (isVisible can) $
  let
    next = nextNode lg
    (u,v) = extractPair can
    addForLabel l g = let
        preds = extractPreds l can
        succs = extractSuccs l can
        reflexive = [(next,next) | u `Set.member` preds || v `Set.member` preds]
        ingoing = [(u,next) | u <- Set.toList preds]
        outgoing = [(next,u) | u <- Set.toList succs]
        withReflexive = lMapAddArcs g l reflexive
        withIngoing = lMapAddArcs withReflexive l ingoing
        withAll = lMapAddArcs withIngoing l outgoing
      in withAll
    withNode = lMapAddNodes (graph lg) [next]
    newGraph = addForLabel One (addForLabel Zero withNode)
    newJustification = insert next (doubleton' u v) (justification lg)
  in (next,LiftedGraph newGraph newJustification (printBase lg))
  
combine :: Int -> Int -> State (LiftedGraph x) Int
combine x y = do
  lg <- get
  let can = computeCandidate (graph lg) (x,y)
  if isVisible can
    then liftCandidate can
    else error ("Trying to lift invisible candidate " ++ show (extractPair can))

prettyLiftedGraph :: LiftedGraph x -> [String]
prettyLiftedGraph lg = let
    justifiedNodePrinter i = (show i) ++ " " ++ just (justify lg i) where
      just (Base x) = "! " ++ printBase lg x
      just (Doubleton m n) = "[" ++ show m ++ " " ++ show n ++ "]"
    setPrinter i = show i
  in prettierBigLabeledGraph intGraphI (graph lg) justifiedNodePrinter setPrinter

noFilter :: IntGraph -> LiftingCandidate -> Bool
noFilter lg can = True

dominationFilter :: IntGraph -> LiftingCandidate -> Bool
dominationFilter ig can = not $ any dominatesCan (domain intGraphI ig) where
  pred = predecessors intGraphI ig
  succ = successors intGraphI ig
  dominatesCan oldNode =
    extractSuccs Zero can `Set.isSubsetOf` succ Zero oldNode &&
    extractSuccs One  can `Set.isSubsetOf` succ One  oldNode &&
    extractPreds Zero can `Set.isSubsetOf` pred Zero oldNode &&
    extractPreds One  can `Set.isSubsetOf` pred One  oldNode

weakDominationFilter :: IntGraph -> LiftingCandidate -> Bool
weakDominationFilter ig can = not $ (any (dominatesCan Zero) dom &&
                                     any (dominatesCan One) dom) where
  pred = predecessors intGraphI ig
  succ = successors intGraphI ig
  dom = domain intGraphI ig
  dominatesCan label oldNode =
    extractPreds Zero can `Set.isSubsetOf` pred Zero oldNode &&
    extractPreds One  can `Set.isSubsetOf` pred One  oldNode &&
    extractSuccs label can `Set.isSubsetOf` succ label oldNode 
