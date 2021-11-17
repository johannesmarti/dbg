module AssocGraph (
  AssocGraph,
  assocGraphI,
  graphAsAssocGraph,
  fromGraph,
  assocToMap,
  materialize,
  subgraph,
  applyBijection,
) where

import Control.Exception.Base
import qualified Data.Set as Set
import qualified Data.Set.Extra

import qualified MapGraph
import qualified Graph

type AssocGraph a = Graph.Label -> [(a,a)]

assocGraphI :: Ord a => Graph.GraphI (AssocGraph a) a
assocGraphI = Graph.GraphI domain successors predecessors

domain :: Ord a => AssocGraph a -> Set.Set a
domain aList = Data.Set.Extra.concatMap ldom Graph.labels where
  ldom l = Set.fromList (fmap fst (aList l)) `Set.union`
           Set.fromList (fmap snd (aList l))

successors :: Ord a => AssocGraph a -> Graph.MapFunction a
successors aList l v = Set.fromList [t | (s,t) <- aList l, s == v]

predecessors :: Ord a => AssocGraph a -> Graph.MapFunction a
predecessors aList l v = Set.fromList [t | (t,s) <- aList l, s == v]

graphAsAssocGraph :: Ord a => Graph.GraphI g a -> g -> AssocGraph a
graphAsAssocGraph gi g = Graph.arcsOfLabel gi g

{- The point of fromGraph as opposed to graphsAsAssocGraph is to actually store the assoc list representation of the graph instead of recomputing it on demand! -}
fromGraph :: Ord a => Graph.GraphI g a -> g -> AssocGraph a
fromGraph gi g = materialize $ graphAsAssocGraph gi g

isNubList :: Eq a => [a] -> Bool
isNubList list = worker [] list where
  worker _ [] = True
  worker seen (next:rest) = not (next `elem` seen) && (worker (next:seen) rest)

isNubby :: Eq a => AssocGraph a -> Bool
isNubby ag = isNubList (ag Graph.Zero) && isNubList (ag Graph.One)

materialize :: Eq a => AssocGraph a -> AssocGraph a
materialize ag = let
    zeroArcs = ag Graph.Zero
    oneArcs  = ag Graph.One
    newGraph Graph.Zero = zeroArcs
    newGraph Graph.One  = oneArcs
  in assert (isNubby newGraph) $ newGraph

applyBijection :: Eq b => (a -> b) -> AssocGraph a -> AssocGraph b
applyBijection b assocGraph = materialize $ mapper . assocGraph where
  liftedb (u,v) = (b u, b v)
  mapper = map liftedb

subgraph :: Ord a => Set.Set a -> AssocGraph a -> AssocGraph a
subgraph set aGraph = materialize $ (\l -> filter pred (aGraph l)) where
  pred (x,y) = x `Set.member` set && y `Set.member` set

assocToMap :: Ord a => AssocGraph a -> MapGraph.MapGraph a
assocToMap = MapGraph.fromGraph assocGraphI

