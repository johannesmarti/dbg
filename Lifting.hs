module Lifting (
  toLiftedGraph,
  liftedGraphI,
  lift,
  lifting,
) where

import Control.Exception.Base
import qualified Data.Set as Set
import qualified Data.List.Extra as ListExtra

import AssocGraph
import Graph

data LiftedNode x = BaseNode x | Singleton (LiftedNode x)
                               | Doubleton (LiftedNode x) (LiftedNode x)
  deriving (Eq,Ord)

prettyLifted :: (x -> String) -> LiftedNode x -> String
prettyLifted prettyBase (BaseNode a) = prettyBase a
prettyLifted prettyBase (Singleton u) =
  '[' : ((prettyLifted prettyBase u) ++ "]")
prettyLifted prettyBase (Doubleton u v) =
  '[' : ((prettyLifted prettyBase u) ++ "," ++ (prettyLifted prettyBase v) ++ "]")

instance Show x => Show (LiftedNode x) where
  show lifted = prettyLifted show lifted

depth :: Ord x => LiftedNode x -> Int
depth (BaseNode _) = 0
depth (Singleton u) = depth u + 1
depth (Doubleton u v) = let du = depth u in assert (du == depth v) $
                                            assert (u < v) $ du + 1

liftedRelation :: (a -> a -> Bool) -> (LiftedNode a) -> (LiftedNode a) -> Bool
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

isCovered :: Eq a => (LiftedNode a) -> (LiftedNode a) -> Bool
isCovered (BaseNode a) (BaseNode b) = a == b
isCovered (Singleton x) (Singleton y) = isCovered x y
isCovered (Singleton x) (Doubleton y y') = isCovered x y || isCovered x y'
isCovered (Doubleton x x') (Singleton y) = isCovered x y && isCovered x' y
isCovered (Doubleton x x') (Doubleton y y') =
  (isCovered x y || isCovered x y') && (isCovered x' y || isCovered x' y')
isCovered _ _ = error "comparing unbalanced lifted nodes"

type LiftedGraph x = AssocGraph (LiftedNode x)

liftedGraphI :: Ord x => GraphI (LiftedGraph x) (LiftedNode x)
liftedGraphI = assocGraphI

balanced :: Ord x => LiftedGraph x -> Bool
balanced liftedGraph = let
    dom = Set.toList $ domain assocGraphI liftedGraph
    depths = map depth dom
  in ListExtra.allSame depths

toLiftedGraph :: Ord x => GraphI g x -> g -> LiftedGraph x
toLiftedGraph gi g = let
    hasPredecessorsDom = Set.filter (\node -> not (noPredecessor gi g node))
                                    (domain gi g)
  in applyBijection BaseNode $
      AssocGraph.subgraph hasPredecessorsDom (AssocGraph.fromGraph gi g)

strictPairs :: [x] -> [(x,x)]
strictPairs list = worker list [] where
  worker [] accum = accum
  worker (next:rest) accum = innerWorker next rest rest accum
  innerWorker elem [] rest accum = worker rest accum
  innerWorker elem (p:ps) rest accum = innerWorker elem ps rest ((elem, p):accum)

graph :: (a -> b) -> [a] -> [(a, b)]
graph _ [] = []
graph f (a:as) = (a,f a) : (graph f as)

lift :: Ord x => LiftedGraph x -> Maybe (LiftedGraph x)
lift agraph = assert (balanced agraph) $ let
    oldDomList = Set.toList $ (domain liftedGraphI agraph)
    pred = predecessors liftedGraphI agraph
    allDoubles = strictPairs oldDomList
    intersecter (a,b) = (pred Zero a `Set.intersection` pred Zero b,
                         pred One a `Set.intersection` pred One b)
    candidates = filter (doIntersect . snd) (graph intersecter allDoubles)
    doIntersect (sa,sb) = not (Set.null sa) && not (Set.null sb)
    toAdd = filter notDominated candidates
    notDominated ((a,b),(pz,po)) = not $ any dominates oldDomList where
            dominates oldNode = a `isCovered` oldNode && b `isCovered` oldNode
                                && (pz `Set.isSubsetOf` pred Zero oldNode &&
                                    po `Set.isSubsetOf` pred One oldNode)
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

lifting :: Ord x => GraphI g x -> g -> AssocGraph (x,x)
lifting gi g l = edges where
  oldDom = domain gi g
  allPairsOfOld = Set.cartesianProduct oldDom oldDom
  dom = Set.filter (\(a,b) -> a <= b) allPairsOfOld
  allPairs = Set.toList (Set.cartesianProduct dom dom)
  edges = filter sees allPairs
  sees ((v,v'),(u,u')) = let sucv  = successors gi g l v
                             sucv' = successors gi g l v'
                           in (u `Set.member` sucv  && u' `Set.member` sucv) ||
                              (u `Set.member` sucv' && u' `Set.member` sucv')
