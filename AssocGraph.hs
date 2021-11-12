module AssocGraph (
  AssocGraph,
  assocGraphI,
) where

import Data.Set as Set
import Data.Set.Extra

import qualified Graph

type AssocGraph a = Graph.Label -> [(a,a)]

assocGraphI :: Ord a => Graph.GraphI (AssocGraph a) a
assocGraphI = Graph.GraphI domain successors predecessors

domain :: Ord a => AssocGraph a -> Set a
domain aList = Data.Set.Extra.concatMap ldom Graph.labels where
  ldom l = Set.fromList (fmap fst (aList l)) `union`
           Set.fromList (fmap snd (aList l))

successors :: Ord a => AssocGraph a -> Graph.MapFunction a
successors aList l v = Set.fromList [t | (s,t) <- aList l, s == v]

predecessors :: Ord a => AssocGraph a -> Graph.MapFunction a
predecessors aList l v = Set.fromList [t | (t,s) <- aList l, s == v]
