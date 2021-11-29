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

lMapAddNodes :: Ord x => LMapGraph x -> [((Set.Set x, Set.Set x), x, (Set.Set x, Set.Set x))] -> LMapGraph x
lMapAddNodes = undefined

liftWithFilter :: Ord x => (LiftedGraph x -> LiftingCandidate (Lifted x) -> Bool)
                           -> (LiftedGraph x) -> Maybe (LiftedGraph x)
liftWithFilter newNodeFilter graph = assert (balanced graph) $ let
    liftedPairs = liftedPairsWithPred liftedGraphINotPretty graph
    candidatesWithPred = filter (newNodeFilter graph) liftedPairs
    candidates = map fst candidatesWithPred
    doubler (x,y) = Doubleton x y
    liftUp ((x,y), pd) = let
        plainPredOfL Zero = fst pd
        plainPredOfL One = snd pd
        forLabel l = let
            succ = successors liftedGraphINotPretty graph l
            plainXsucc = succ x
            plainYsucc = succ y
            plainDsucc = filter bothInOne candidates
            bothInOne (u,v) =
              (u `Set.member` plainXsucc && v `Set.member` plainXsucc) ||
              (u `Set.member` plainYsucc && v `Set.member` plainYsucc)
            singleSucc = Set.map Singleton (plainXsucc `Set.union` plainYsucc)
            doubleSucc = Set.fromList $ map doubler plainDsucc
            plainPred = plainPredOfL l
            plainDPred = filter oneIsPred candidates
            oneIsPred (u,v) = u `Set.member` plainPred ||
                              v `Set.member` plainPred
            singlePred = Set.map Singleton plainPred
            doublePred = Set.fromList $ (map doubler plainDPred)
          in (singlePred `Set.union` doublePred,doubleSucc `Set.union` singleSucc)
        (zp,zs) = forLabel Zero
        (op,os) = forLabel One
      in ((zp,op), Doubleton x y, (zs,os))
    newNodes = map liftUp candidatesWithPred
    liftedOld = lMapApplyBijection liftedGraphINotPretty graph Singleton
  in if null candidates
       then Nothing
       else Just $ lMapAddNodes liftedOld newNodes


lift :: Ord x => LiftedGraph x -> Maybe (LiftedGraph x)
lift = liftWithFilter (\_ _ -> True)
