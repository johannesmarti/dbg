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

liftedNodesWithPred :: Ord x => LabeledGraphI g x -> g -> [((x,x), (Set.Set x, Set.Set x))]
liftedNodesWithPred gi g = let
    domList = Set.toList $ domain gi g
    pred = predecessors gi g
    alldoubles = strictPairs domList
    intersecter (a,b) = (pred Zero a `Set.intersection` pred Zero b,
                         pred One a `Set.intersection` pred One b)
    graph f list = map (\x -> (x,f(x))) list
    candidates = filter (dointersect . snd) (graph intersecter alldoubles)
    dointersect (sa,sb) = not (Set.null sa) && not (Set.null sb)
  in candidates

liftedNodes :: Ord x => LabeledGraphI g x -> g -> [(x,x)]
liftedNodes gi g = map fst $ liftedNodesWithPred gi g

{-
lift :: Ord x => LiftedGraph x -> Maybe (LiftedGraph x)
lift agraph = assert (balanced agraph) $ let
    olddomlist = set.tolist $ (domain liftedgraphi agraph)
    setcondi = set.issubsetof
    pred = predecessors liftedgraphi agraph
    alldoubles = strictpairs olddomlist
    intersecter (a,b) = (pred zero a `set.intersection` pred zero b,
                         pred one a `set.intersection` pred one b)
    candidates = filter (dointersect . snd) (graph intersecter alldoubles)
    dointersect (sa,sb) = not (set.null sa) && not (set.null sb)
    toadd = filter notdominated candidates
    notdominated ((a,b),(pz,po)) = not $ any dominates olddomlist where
            dominates oldnode = a `iscovered` oldnode && b `iscovered` oldnode
                                && (pz `setcondi` pred zero oldnode &&
                                    po `setcondi` pred one oldnode)
    newnodeswithpreds = map (\((a,b),(pz,po)) -> (doubleton a b,
                             (set.map singleton pz,
                              set.map singleton po))) toadd
    newnodes = map fst newnodeswithpreds
    liftedold = applybijection singleton agraph
    withnewnodes = addnodeswithpreds newnodeswithpreds liftedold
    liftedgraph = addnodeswithsuccs succsofnewnodes withnewnodes
    succsofnewnodes = map gaga newnodes
    gaga d = let doubleton a b = d
                 succs = successors liftedgraphi withnewnodes
                 zsuc = succs graph.zero (singleton a) `set.union` 
                        succs graph.zero (singleton b)
                 osuc = succs graph.one (singleton a) `set.union` 
                        succs graph.one (singleton b)
      in (d,(zsuc,osuc))
  in if null toadd
       then nothing
       else just liftedgraph
-}
