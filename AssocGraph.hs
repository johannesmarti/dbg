module AssocGraph (
  AssocGraph,
  assocGraphI,
  graphAsAssocGraph,
  fromGraph,
  assocToMap,
  materialize,
  subgraph,
  applyBijection,
  addNodesWithSuccs,
  addNodesWithPreds,
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
materialize ag = fromPair (ag Graph.Zero, ag Graph.One)

fromPair :: Eq a => ([(a,a)],[(a,a)]) -> AssocGraph a
fromPair (zList,oList) = assert (isNubby newGraph) $ newGraph where
  newGraph Graph.Zero = zList
  newGraph Graph.One = oList


applyBijection :: Eq b => (a -> b) -> AssocGraph a -> AssocGraph b
applyBijection b assocGraph = materialize $ mapper . assocGraph where
  liftedb (u,v) = (b u, b v)
  mapper = map liftedb

subgraph :: Ord a => Set.Set a -> AssocGraph a -> AssocGraph a
subgraph set aGraph = materialize $ (\l -> filter pred (aGraph l)) where
  pred (x,y) = x `Set.member` set && y `Set.member` set

assocToMap :: Ord a => AssocGraph a -> MapGraph.MapGraph a
assocToMap = MapGraph.fromGraph assocGraphI

addNodesWithSuccs :: Ord a => [(a,(Set.Set a, Set.Set a))] -> AssocGraph a -> AssocGraph a
addNodesWithSuccs toAdd graph = fromPair $
  (foldl (\list (n,(sz,_)) -> [(n,v) | v <- Set.toList sz] ++ list) (graph Graph.Zero) toAdd,
   foldl (\list (n,(_,so)) -> [(n,v) | v <- Set.toList so] ++ list) (graph Graph.One) toAdd)

addNodesWithPreds :: Ord a => [(a,(Set.Set a, Set.Set a))] -> AssocGraph a -> AssocGraph a
addNodesWithPreds toAdd graph = fromPair $
  (foldl (\list (n,(sz,_)) -> [(v,n) | v <- Set.toList sz] ++ list) (graph Graph.Zero) toAdd,
   foldl (\list (n,(_,so)) -> [(v,n) | v <- Set.toList so] ++ list) (graph Graph.One) toAdd)

