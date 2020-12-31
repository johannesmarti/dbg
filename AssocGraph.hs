module AssocGraph (
  AssocGraph,
  toGraph,
) where

import Data.Set as Set
import Data.Set.Extra

import Graph

type AssocGraph a = Label -> [(a,a)]

toGraph :: Ord a => AssocGraph a -> Graph a
toGraph aList = Graph.fromFunctions dom succ pred where
  ldom l = Set.fromList (fmap fst (aList l)) `union` Set.fromList (fmap snd (aList l))
  dom = Data.Set.Extra.concatMap ldom labels
  succ l v = Set.fromList [t | (s,t) <- aList l, s == v]
  pred l v = Set.fromList [t | (t,s) <- aList l, s == v]

