module Lifting (
  LiftedGraph,
  liftedGraphI,
  toLiftedGraph,
  --lift,
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

{-
graph :: (a -> b) -> [a] -> [(a, b)]
graph _ [] = []
graph f (a:as) = (a,f a) : (graph f as)

lift :: Ord x => LiftedGraph x -> Maybe (LiftedGraph x)
lift agraph = assert (balanced agraph) $ let
    oldDomList = Set.toList $ (domain liftedGraphI agraph)
    setCondi = Set.isSubsetOf
    pred = predecessors liftedGraphI agraph
    allDoubles = strictPairs oldDomList
    intersecter (a,b) = (pred Zero a `Set.intersection` pred Zero b,
                         pred One a `Set.intersection` pred One b)
    candidates = filter (doIntersect . snd) (graph intersecter allDoubles)
    doIntersect (sa,sb) = not (Set.null sa) && not (Set.null sb)
    toAdd = filter notDominated candidates
    notDominated ((a,b),(pz,po)) = not $ any dominates oldDomList where
            dominates oldNode = a `isCovered` oldNode && b `isCovered` oldNode
                                && (pz `setCondi` pred Zero oldNode &&
                                    po `setCondi` pred One oldNode)
    newNodesWithPreds = map (\((a,b),(pz,po)) -> (Doubleton a b,
                             (Set.map Singleton pz,
                              Set.map Singleton po))) toAdd
    newNodes = map fst newNodesWithPreds
    liftedOld = applyBijection Singleton agraph
    withNewNodes = addNodesWithPreds newNodesWithPreds liftedOld
    liftedGraph = addNodesWithSuccs succsOfNewNodes withNewNodes
    succsOfNewNodes = map gaga newNodes
    gaga d = let Doubleton a b = d
                 succs = successors liftedGraphI withNewNodes
                 zsuc = succs Graph.Zero (Singleton a) `Set.union` 
                        succs Graph.Zero (Singleton b)
                 osuc = succs Graph.One (Singleton a) `Set.union` 
                        succs Graph.One (Singleton b)
      in (d,(zsuc,osuc))
  in if null toAdd
       then Nothing
       else Just liftedGraph
-}
