module Lifting (
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

covers :: Eq a => (LiftedNode a) -> (LiftedNode a) -> Bool
covers (BaseNode a) (BaseNode b) = a == b
covers (Singleton x) (Singleton y) = covers x y
covers (Singleton x) (Doubleton y y') = covers x y || covers x y'
covers (Doubleton x x') (Singleton y) = covers x y && covers x' y
covers (Doubleton x x') (Doubleton y y') =
  (covers x y || covers x y') && (covers x' y || covers x' y')
covers _ _ = error "comparing unbalanced lifted nodes"

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

lift :: Ord x => LiftedGraph x -> LiftedGraph x
lift agraph = assert (balanced agraph) $ undefined


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
