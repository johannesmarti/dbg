module Lifting (
  LiftedGraph,
  liftedGraphI,
  liftedGraphINotPretty,
  liftedGraphIWithNodePrinter,
  toLiftedGraph,
  lift,
  liftWithFilter,
  noFilter,
  dominationFilter,
  weakDominationFilter,
  unsoundDominationFilter,
) where

import Control.Exception.Base (assert)
import qualified Data.Set as Set
import qualified Data.List.Extra as ListExtra

import CommonLGraphTypes
import LabeledGraph
import Lifted
import Pretty

type LiftedGraph x = LMapGraph (Lifted x)

liftedGraphI :: (Ord x, Pretty x) => LabeledGraphI (LiftedGraph x) (Lifted x)
liftedGraphI = lMapGraphI

liftedGraphIWithNodePrinter :: Ord x => (x -> String) -> LabeledGraphI (LiftedGraph x) (Lifted x)
liftedGraphIWithNodePrinter printer = lMapGraphIWithNodePrinter (prettyLifted printer)

liftedGraphINotPretty :: Ord x => LabeledGraphI (LiftedGraph x) (Lifted x)
liftedGraphINotPretty = lMapGraphINotPretty

balanced :: Ord x => LiftedGraph x -> Bool
balanced liftedGraph = let
    dom = Set.toList $ domain liftedGraphINotPretty liftedGraph
    depths = map depth dom
  in ListExtra.allSame depths


toLiftedGraph :: Ord x => LabeledGraphI g x -> g -> LiftedGraph x
toLiftedGraph gi g = assert (not (any (noPredecessor gi g) (domain gi g))) $
  lMapApplyBijection gi g BaseNode 

type LiftingCandidate x = ((Set.Set x, Set.Set x), (x,x), (Set.Set x, Set.Set x))

extractPair :: LiftingCandidate x -> (x,x)
extractPair (pre,pair,suc) = pair

extractPreds :: Label -> LiftingCandidate x -> Set.Set x
extractPreds Zero (pre,_,_) = fst pre
extractPreds One  (pre,_,_) = snd pre

extractSuccs :: Label -> LiftingCandidate x -> Set.Set x
extractSuccs Zero (_,_,suc) = fst suc
extractSuccs One  (_,_,suc) = snd suc 

strictPairs :: [x] -> [(x,x)]
strictPairs list = worker list [] where
  worker [] accum = accum
  worker (next:rest) accum = innerWorker next rest rest accum
  innerWorker elem [] rest accum = worker rest accum
  innerWorker elem (p:ps) rest accum = innerWorker elem ps rest ((elem, p):accum)

liftedPairsWithPS :: Ord x => LabeledGraphI g x -> g -> [LiftingCandidate x]
liftedPairsWithPS gi g = let
    domList = Set.toList $ domain gi g
    succ = successors gi g
    pred = predecessors gi g
    alldoubles = strictPairs domList
    improver (a,b) = ((pred Zero a `Set.intersection` pred Zero b,
                       pred One a `Set.intersection` pred One b),
                      (a,b),
                      (succ Zero a `Set.union` succ Zero b,
                       succ One a `Set.union` succ One b))
    candidates = filter (dointersect) (map improver alldoubles)
    dointersect can = not (Set.null $ extractPreds Zero can) && not (Set.null $ extractPreds One can)
  in candidates

liftedPairs :: Ord x => LabeledGraphI g x -> g -> [(x,x)]
liftedPairs gi g = map extractPair $ liftedPairsWithPS gi g

liftWithFilter :: Ord x => (LiftedGraph x -> LiftingCandidate (Lifted x) -> Bool)
                           -> (LiftedGraph x) -> Maybe (LiftedGraph x)
liftWithFilter newNodeFilter graph = assert (balanced graph) $ let
    liftedPairs = liftedPairsWithPS liftedGraphINotPretty graph
    candidatesWithPS = filter (newNodeFilter graph) liftedPairs
    candidates = map extractPair candidatesWithPS
    newNodes = map doubler candidates
    doubler (x,y) = du x y
    fromOldEdges l = concatMap (fromOldForNode l) candidatesWithPS
    fromOldForNode l can = let
        (x,y) = extractPair can
        preds = Set.toList $ extractPreds l can
        arcs = map (\p -> (si p, du x y)) preds
      in arcs
    toOldEdges l = concatMap (toOldForNode l) candidatesWithPS
    toOldForNode l can = let
        (x,y) = extractPair can
        succs = Set.toList $ extractSuccs l can
        arcs = map (\p -> (du x y, si p)) succs
      in arcs
    -- this between could maybe be added to fromOld!
    betweenNewEdges l = concatMap (fromNewForNode l) candidatesWithPS
    fromNewForNode l can = let
        (x,y) = extractPair can
        preds = extractPreds l can
        plainDPreds = filter oneIsPred candidates
        oneIsPred (u,v) = u `Set.member` preds ||
                          v `Set.member` preds
        arcs = map (\(u,v) -> (du u v, du x y)) plainDPreds
      in arcs
    newArcsForLabel l = fromOldEdges l ++ toOldEdges l ++ betweenNewEdges l
    liftedOld = lMapApplyBijection liftedGraphINotPretty graph Singleton
    withNewNodes = lMapAddNodes liftedOld newNodes
    withZeroArcs = lMapAddArcs withNewNodes Zero (newArcsForLabel Zero)
    newGraph = lMapAddArcs withZeroArcs One (newArcsForLabel One)
  in if null newNodes
       then Nothing
       else Just $ newGraph

noFilter :: Ord x => LiftedGraph x -> LiftingCandidate (Lifted x) -> Bool
noFilter lg can = True

dominationFilter :: Ord x => LiftedGraph x -> LiftingCandidate (Lifted x) -> Bool
dominationFilter lg can = not $ any dominatesCan (domain liftedGraphINotPretty lg) where
  pred = predecessors liftedGraphINotPretty lg
  succ = successors liftedGraphINotPretty lg
  dominatesCan oldNode =
    extractSuccs Zero can `Set.isSubsetOf` succ Zero oldNode &&
    extractSuccs One  can `Set.isSubsetOf` succ One  oldNode &&
    extractPreds Zero can `Set.isSubsetOf` pred Zero oldNode &&
    extractPreds One  can `Set.isSubsetOf` pred One  oldNode

weakDominationFilter :: Ord x => LiftedGraph x -> LiftingCandidate (Lifted x) -> Bool
weakDominationFilter lg can = not $ (any (dominatesCan Zero) dom &&
                                     any (dominatesCan One) dom) where
  pred = predecessors liftedGraphINotPretty lg
  succ = successors liftedGraphINotPretty lg
  dom = domain liftedGraphINotPretty lg
  dominatesCan label oldNode =
    extractPreds Zero can `Set.isSubsetOf` pred Zero oldNode &&
    extractPreds One  can `Set.isSubsetOf` pred One  oldNode &&
    extractSuccs label can `Set.isSubsetOf` succ label oldNode 

unsoundDominationFilter :: Ord x => LiftedGraph x -> LiftingCandidate (Lifted x) -> Bool
unsoundDominationFilter lg can = not $ (any (dominatesCan Zero) dom &&
                                     any (dominatesCan One) dom) where
  pred = predecessors liftedGraphINotPretty lg
  succ = successors liftedGraphINotPretty lg
  dom = domain liftedGraphINotPretty lg
  dominatesCan label oldNode =
    extractSuccs label can `Set.isSubsetOf` succ label oldNode 

lift :: Ord x => LiftedGraph x -> Maybe (LiftedGraph x)
lift = liftWithFilter (\_ _ -> True)
