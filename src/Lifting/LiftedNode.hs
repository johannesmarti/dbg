module Lifting.LiftedNode (
  Lifted,
  bn, si, du,
  du',
  depth,
  deepen,
  prettyLifted,
  liftedRelation,
) where

import Control.Exception.Base (assert)

import Graphs.PrettyNode

data Lifted x = BaseNode x | Singleton (Lifted x)
                           | Doubleton (Lifted x) (Lifted x)
  deriving (Eq,Ord)

prettyLifted :: (x -> String) -> Lifted x -> String
prettyLifted prettyBase (BaseNode a) = prettyBase a
prettyLifted prettyBase (Singleton u) =
  '[' : ((prettyLifted prettyBase u) ++ "]")
prettyLifted prettyBase (Doubleton u v) =
  '[' : ((prettyLifted prettyBase u) ++ " " ++ (prettyLifted prettyBase v) ++ "]")

instance Show x => Show (Lifted x) where
  show lifted = prettyLifted show lifted

instance PrettyNode x => PrettyNode (Lifted x) where
  pretty lifted = prettyLifted pretty lifted

bn :: x -> Lifted x
bn = BaseNode

si :: Lifted x -> Lifted x
si = Singleton

-- du' is a version of du that assumes that the arguments are already in order.
du' :: Ord x => Lifted x -> Lifted x -> Lifted x
du' u v = assert (u < v) $
             assert (depth u == depth v) $ Doubleton u v

du :: Ord x => Lifted x -> Lifted x -> Lifted x
du u v = assert (depth u == depth v) $
  if u < v
    then du' u v
    else du' v u

deepen :: Ord x => Lifted x -> Lifted x
deepen (BaseNode a) = si $ bn a
deepen (Singleton x) = si (deepen x)
deepen (Doubleton x y) = du (deepen x) (deepen y)


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
