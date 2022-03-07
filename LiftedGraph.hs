module LiftedGraph (

) where

import Control.Exception.Base (assert)
import Control.Monad.State.Lazy
import Data.Map.Strict as Map hiding (filter, map)
import qualified Data.Set as Set

import CommonLGraphTypes
import LabeledGraph
import Lifted
import Tools (strictPairs)

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
  justification :: Map Int (Justification x)
}

allJustified :: LiftedGraph x -> Bool
allJustified lg = keysSet (justification lg) == domain intGraphI (graph lg)

justify :: LiftedGraph x -> Int -> Justification x
justify lg node =
  case Map.lookup node (justification lg) of
    Just j  -> j
    Nothing -> error "node is not in LiftedGraph"

topNode :: LiftedGraph x -> Int
topNode lg = case Set.lookupMax (domain intGraphI (graph lg)) of
               Just m  ->  m
               Nothing -> -1

nextNode :: LiftedGraph x -> Int
nextNode lg = topNode lg + 1

type LiftingCandidate x = ((Set.Set x, Set.Set x), (x,x), (Set.Set x, Set.Set x))

extractPair :: LiftingCandidate x -> (x,x)
extractPair (pre,pair,suc) = pair

extractPreds :: Label -> LiftingCandidate x -> Set.Set x
extractPreds Zero (pre,_,_) = fst pre
extractPreds One  (pre,_,_) = snd pre

extractSuccs :: Label -> LiftingCandidate x -> Set.Set x
extractSuccs Zero (_,_,suc) = fst suc
extractSuccs One  (_,_,suc) = snd suc 

computeCandidate :: Ord x => LabeledGraphI g x -> g -> (x,x)
                             -> LiftingCandidate x
computeCandidate gi g (a,b) =
  ((pred Zero a `Set.intersection` pred Zero b,
    pred One a `Set.intersection` pred One b),
   (a,b),
   (succ Zero a `Set.union` succ Zero b,
    succ One a `Set.union` succ One b)) where
      succ = successors gi g
      pred = predecessors gi g

isVisible :: LiftingCandidate x -> Bool
isVisible can = (Set.null $ extractPreds Zero can) && not (Set.null $ extractPreds One can)

liftableCandidates :: Ord x => LabeledGraphI g x -> g -> [LiftingCandidate x]
liftableCandidates gi g = let
    domList = Set.toList $ domain gi g
    alldoubles = strictPairs domList
    candidates = filter isVisible (map (computeCandidate gi g) alldoubles)
  in candidates

liftablePairs :: Ord x => LabeledGraphI g x -> g -> [(x,x)]
liftablePairs gi g = map extractPair $ liftableCandidates gi g

liftCandidate :: LiftingCandidate Int -> State (LiftedGraph x) Int
liftCandidate can = state $ \lg ->
  assert (can == computeCandidate intGraphI (graph lg) (extractPair can)) $
  assert (isVisible can) $
  let
    next = nextNode lg
    (u,v) = extractPair can
    addForLabel l g = let
        preds = extractPreds l can
        succs = extractSuccs l can
        reflexive = [(next,next) | u `Set.member` preds && v `Set.member` preds]
        ingoing = [(u,next) | u <- Set.toList preds]
        outgoing = [(next,u) | u <- Set.toList succs]
        withReflexive = lMapAddArcs g l reflexive
        withIngoing = lMapAddArcs withReflexive l ingoing
        withAll = lMapAddArcs withIngoing l outgoing
      in withAll
    withNode = lMapAddNodes (graph lg) [next]
    newGraph = addForLabel One (addForLabel Zero withNode)
    newJustification = insert next (doubleton' u v) (justification lg)
  in (next,LiftedGraph newGraph newJustification)
  
combine :: Int -> Int -> State (LiftedGraph x) Int
combine x y = do
  lg <- get
  let can = computeCandidate intGraphI (graph lg) (x,y)
  liftCandidate can

