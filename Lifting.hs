module Lifting (
  LiftedGraph,
  liftedGraphI,
  toLiftedGraph,
  lift,
) where

import Control.Exception.Base
import qualified Data.Set as Set
import qualified Data.List.Extra as ListExtra

import CommonLGraphTypes
import LabeledGraph
import Pretty

data Lifted x = BaseNode x | Singleton (Lifted x)
                           | Doubleton (Lifted x) (Lifted x)
  deriving (Eq,Ord)

prettyLifted :: (x -> String) -> Lifted x -> String
prettyLifted prettyBase (BaseNode a) = prettyBase a
prettyLifted prettyBase (Singleton u) =
  '[' : ((prettyLifted prettyBase u) ++ "]")
prettyLifted prettyBase (Doubleton u v) =
  '[' : ((prettyLifted prettyBase u) ++ (prettyLifted prettyBase v) ++ "]")

instance Show x => Show (Lifted x) where
  show lifted = prettyLifted show lifted

instance Pretty x => Pretty (Lifted x) where
  pretty lifted = prettyLifted pretty lifted

instance Functor Lifted where
  fmap f (BaseNode a) = BaseNode (f a)
  fmap f (Singleton x) = Singleton (fmap f x)
  fmap f (Doubleton x y) = Doubleton (fmap f x) (fmap f y)

depth :: Ord x => Lifted x -> Int
depth (BaseNode _) = 0
depth (Singleton u) = depth u + 1
depth (Doubleton u v) = let du = depth u in assert (du == depth v) $
                                            assert (u < v) $ du + 1

liftedRelation :: (a -> a -> Bool) -> (Lifted a) -> (Lifted a) -> Bool
liftedRelation baseRel (BaseNode a) (BaseNode b) = baseRel a b
liftedRelation baseRel (Singleton x) (Singleton y) = liftedRelation baseRel x y
liftedRelation baseRel (Singleton x) (Doubleton y y') =
  liftedRelation baseRel x y && liftedRelation baseRel x y'
liftedRelation baseRel (Doubleton x x') (Singleton y) =
  liftedRelation baseRel x y || liftedRelation baseRel x' y
liftedRelation baseRel (Doubleton x x') (Doubleton y y') =
  (liftedRelation baseRel x y && liftedRelation baseRel x y') ||
  (liftedRelation baseRel x' y && liftedRelation baseRel x' y')
liftedRelation baseRel _ _ = error "comparing unbalanced lifted nodes"

isCovered :: Eq a => (Lifted a) -> (Lifted a) -> Bool
isCovered (BaseNode a) (BaseNode b) = a == b
isCovered (Singleton x) (Singleton y) = isCovered x y
isCovered (Singleton x) (Doubleton y y') = isCovered x y || isCovered x y'
isCovered (Doubleton x x') (Singleton y) = isCovered x y && isCovered x' y
isCovered (Doubleton x x') (Doubleton y y') =
  (isCovered x y || isCovered x y') && (isCovered x' y || isCovered x' y')
isCovered _ _ = error "comparing unbalanced lifted nodes"

type LiftedGraph x = LMapGraph (Lifted x)

liftedGraphI :: (Ord x, Pretty x) => LabeledGraphI (LiftedGraph x) (Lifted x)
liftedGraphI = lMapGraphI

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

strictPairs :: [x] -> [(x,x)]
strictPairs list = worker list [] where
  worker [] accum = accum
  worker (next:rest) accum = innerWorker next rest rest accum
  innerWorker elem [] rest accum = worker rest accum
  innerWorker elem (p:ps) rest accum = innerWorker elem ps rest ((elem, p):accum)

type LiftingCandidate x = ((x,x), (Set.Set x, Set.Set x))

liftedPairsWithPred :: Ord x => LabeledGraphI g x -> g -> [LiftingCandidate x]
liftedPairsWithPred gi g = let
    domList = Set.toList $ domain gi g
    pred = predecessors gi g
    alldoubles = strictPairs domList
    intersecter (a,b) = (pred Zero a `Set.intersection` pred Zero b,
                         pred One a `Set.intersection` pred One b)
    graph f list = map (\x -> (x,f(x))) list
    candidates = filter (dointersect . snd) (graph intersecter alldoubles)
    dointersect (sa,sb) = not (Set.null sa) && not (Set.null sb)
  in candidates

liftedPairs :: Ord x => LabeledGraphI g x -> g -> [(x,x)]
liftedPairs gi g = map fst $ liftedPairsWithPred gi g

liftWithFilter :: Ord x => (LiftedGraph x -> LiftingCandidate (Lifted x) -> Bool)
                           -> (LiftedGraph x) -> Maybe (LiftedGraph x)
liftWithFilter newNodeFilter graph = assert (balanced graph) $ let
    liftedPairs = liftedPairsWithPred liftedGraphINotPretty graph
    candidatesWithPred = filter (newNodeFilter graph) liftedPairs
    candidates = map fst candidatesWithPred
    newNodes = map doubler candidates
    doubler (x,y) = Doubleton x y
    fromOldEdges l = concatMap (fromOldForNode l) candidatesWithPred
    fromOldForNode l ((x,y), (pz,po)) = let
        preds = Set.toList $ case l of
                               Zero -> pz
                               One  -> po
        arcs = map (\p -> (Singleton p, Doubleton x y)) preds
      in arcs
    toOldEdges l = concatMap (toOldForNode l) candidates
    toOldForNode l (x,y) = let
        succ = successors liftedGraphINotPretty graph l
        plainXsucc = succ x
        plainYsucc = succ y
        plainSuccs = Set.toList $ (plainXsucc `Set.union` plainYsucc)
        arcs = map (\p -> (Doubleton x y, Singleton p)) plainSuccs
      in arcs
    -- this between could maybe be added to fromOld!
    betweenNewEdges l = concatMap (fromNewForNode l) candidatesWithPred
    fromNewForNode l ((x,y), (pz,po)) = let
        preds = case l of Zero -> pz
                          One  -> po
        plainDPreds = filter oneIsPred candidates
        oneIsPred (u,v) = u `Set.member` preds ||
                          v `Set.member` preds
        arcs = map (\(u,v) -> (Doubleton u v, Doubleton x y)) plainDPreds
      in arcs
    newArcsForLabel l = fromOldEdges l ++ toOldEdges l ++ betweenNewEdges l
    liftedOld = lMapApplyBijection liftedGraphINotPretty graph Singleton
    withNewNodes = lMapAddNodes liftedOld newNodes
    withZeroArcs = lMapAddArcs withNewNodes Zero (newArcsForLabel Zero)
    newGraph = lMapAddArcs withZeroArcs One (newArcsForLabel One)
  in if null newNodes
       then Nothing
       else Just $ newGraph


lift :: Ord x => LiftedGraph x -> Maybe (LiftedGraph x)
lift = liftWithFilter (\_ _ -> True)
