module LiftedGraph (
  LiftedGraph,
  intGraphInterface,
  embed,
  graph,
  LiftedGraph.size,
  fromLabeledGraph,
  fromLabeledGraphWithCoding,
  toLabeledBitGraph,
  liftCandidate,
  combine,
  prettyLiftedGraph,
  LiftingCandidate,
  prettyCandidate,
  prettyCanWithArcs,
  extractPair,
  labeledArcsOfCandidate,
  liftableCandidates,
  liftablePairs,
  noFilter,
  dominationFilter,
  weakDominationFilter,
) where

import Control.Exception.Base (assert)
import Control.Monad.State.Lazy
import Data.Map.Strict as Map hiding (filter, map)
import qualified Data.Set as Set

import Data.Label
import Graphs.BitGraph (Size,fromArcs)
import Coding hiding (domain)
import Graphs.CommonLabeledGraphTypes
import Graphs.LabeledGraphInterface
import Lifted
import Tools (strictPairs)
import Graphs.PrettyNode (stdPrintSet)
import Graphs.PairGraph (fromFunction)

data Justification x = Base x | Doubleton Int Int

base :: x -> Justification x
base a = Base a

doubleton' :: Int -> Int -> Justification x
doubleton' a b = assert (a < b) $ Doubleton a b

doubleton :: Int -> Int -> Justification x
doubleton a b = case compare a b of
  LT -> doubleton' a b
  EQ -> error "Refuse to create doubleton justification of two equal numbers."
  GT -> doubleton' b a

type IntGraph = LabeledMapGraph Int

intGraphInterface :: LabeledGraphInterface IntGraph Int
intGraphInterface = labeledMapGraphInterface

data LiftedGraph x = LiftedGraph {
  graph         :: IntGraph,
  justification :: Map Int (Justification x),
  embed         :: x -> Int,
  printBase     :: x -> String
}

allJustified :: LiftedGraph x -> Bool
allJustified lg = keysSet (justification lg) == domain intGraphInterface (graph lg)

justify :: LiftedGraph x -> Int -> Justification x
justify lg node = assert (allJustified lg) $
  case Map.lookup node (justification lg) of
    Just j  -> j
    Nothing -> error "node is not in LiftedGraph"

topNode :: LiftedGraph x -> Int
topNode lg = case Set.lookupMax (domain intGraphInterface (graph lg)) of
               Just m  ->  m
               Nothing -> -1

size :: LiftedGraph x -> Int
size = nextNode

nextNode :: LiftedGraph x -> Int
nextNode lg = topNode lg + 1

fromLabeledGraph :: Ord x => LabeledGraphInterface g x -> g -> LiftedGraph x
fromLabeledGraph gi g = fst (fromLabeledGraphWithCoding gi g)

fromLabeledGraphWithCoding :: Ord x => LabeledGraphInterface g x -> g
                                 -> (LiftedGraph x, Coding x Int)
fromLabeledGraphWithCoding gi g = (LiftedGraph intGraph just emb pb, coding) where
  coding = codeSet (domain gi g)
  emb = encode coding
  intGraph = labeledMapApplyBijection gi g emb
  just = fromSet justifyBase (domain intGraphInterface intGraph)
  justifyBase i = base (decode coding i)
  pb = prettyNode gi g

toLabeledBitGraph :: LiftedGraph x -> (LabeledBitGraph,Size)
toLabeledBitGraph lg = let
    s = LiftedGraph.size lg
    g = graph lg
  in (Graphs.PairGraph.fromFunction (\l ->
       Graphs.BitGraph.fromArcs s (arcsOfLabel intGraphInterface g l)), s)

type LiftingCandidate = ((Set.Set Int, Set.Set Int), (Int,Int), (Set.Set Int, Set.Set Int))

prettyCandidate :: LiftingCandidate -> String
prettyCandidate ((zp,op),(u,v),(zs,os)) =
    stdPrintSet show zp ++ " 0> " ++ pair ++ "  0> " ++ stdPrintSet show zs ++ "\n"
      ++ stdPrintSet show op ++ " 1> " ++ pair ++ "  1> " ++ stdPrintSet show os ++ "\n" where
        pair = "[" ++ show u ++ " " ++ show v ++ "]"

prettyCanWithArcs :: LiftingCandidate -> String
prettyCanWithArcs can =
  show (extractPair can) ++ " " ++ show (labeledArcsOfCandidate can)

extractPair :: LiftingCandidate -> (Int,Int)
extractPair (pre,pair,suc) = pair

extractPreds :: Label -> LiftingCandidate -> Set.Set Int
extractPreds Zero (pre,_,_) = fst pre
extractPreds One  (pre,_,_) = snd pre

extractSuccs :: Label -> LiftingCandidate -> Set.Set Int
extractSuccs Zero (_,_,suc) = fst suc
extractSuccs One  (_,_,suc) = snd suc 

arcsOfCandidate :: Label -> LiftingCandidate -> [(Int,Int)]
arcsOfCandidate l can = let
    preds = Set.toList $ extractPreds l can
    (u,v) = extractPair can
  in [(p,u) | p <- preds] ++ [(p,v) | p <- preds]

labeledArcsOfCandidate :: LiftingCandidate -> [(Int,Label,Int)]
labeledArcsOfCandidate can = concatMap larcs labels where
  larcs l = [(u,l,v) | (u,v) <- arcsOfCandidate l can]

computeCandidate :: IntGraph -> (Int,Int) -> LiftingCandidate
computeCandidate g (a,b) =
  ((pred Zero a `Set.intersection` pred Zero b,
    pred One a `Set.intersection` pred One b),
   (a,b),
   (succ Zero a `Set.union` succ Zero b,
    succ One a `Set.union` succ One b)) where
      succ = successors intGraphInterface g
      pred = predecessors intGraphInterface g

isVisible :: LiftingCandidate -> Bool
isVisible can = not (Set.null $ extractPreds Zero can) && not (Set.null $ extractPreds One can)

liftableCandidates :: IntGraph -> [LiftingCandidate]
liftableCandidates g = let
    domList = Set.toList $ domain intGraphInterface g
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
        withReflexive = labeledMapAddArcs g l reflexive
        withIngoing = labeledMapAddArcs withReflexive l ingoing
        withAll = labeledMapAddArcs withIngoing l outgoing
      in withAll
    withNode = labeledMapAddNodes (graph lg) [next]
    newGraph = addForLabel One (addForLabel Zero withNode)
    newJustification = insert next (doubleton u v) (justification lg)
  in (next,LiftedGraph newGraph newJustification (embed lg) (printBase lg))
  
combine :: Int -> Int -> State (LiftedGraph x) Int
combine x y = if x == y then return x else do
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
  in prettierBigLabeledGraph intGraphInterface (graph lg) justifiedNodePrinter setPrinter

instance Show (LiftedGraph x) where
  show lg = unlines $ prettyLiftedGraph lg

noFilter :: IntGraph -> LiftingCandidate -> Bool
noFilter _ _ = True

dominationFilter :: IntGraph -> LiftingCandidate -> Bool
dominationFilter ig can = not $ any dominatesCan (domain intGraphInterface ig) where
  pred = predecessors intGraphInterface ig
  succ = successors intGraphInterface ig
  dominatesCan oldNode =
    extractSuccs Zero can `Set.isSubsetOf` succ Zero oldNode &&
    extractSuccs One  can `Set.isSubsetOf` succ One  oldNode &&
    extractPreds Zero can `Set.isSubsetOf` pred Zero oldNode &&
    extractPreds One  can `Set.isSubsetOf` pred One  oldNode

weakDominationFilter :: IntGraph -> LiftingCandidate -> Bool
weakDominationFilter ig can = not $ (any (dominatesCan Zero) dom &&
                                     any (dominatesCan One) dom) where
  pred = predecessors intGraphInterface ig
  succ = successors intGraphInterface ig
  dom = domain intGraphInterface ig
  dominatesCan label oldNode =
    extractPreds Zero can `Set.isSubsetOf` pred Zero oldNode &&
    extractPreds One  can `Set.isSubsetOf` pred One  oldNode &&
    extractSuccs label can `Set.isSubsetOf` succ label oldNode 
