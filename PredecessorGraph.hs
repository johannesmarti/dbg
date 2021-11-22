module PredecessorGraph (
  PredecessorGraph,
  predecessorGraphI,
  fromGraph,
  projection,
) where

import Control.Exception.Base
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Set.Extra as SetExtra

import qualified Graph

data PredecessorGraph x = MapGraph {
  predecessorMap :: Map x (Set x,Set x) 
}

predecessorGraphI :: Ord x => Graph.GraphI (MapGraph x) x
predecessorGraphI = Graph.GraphI domain successors predecessors

successors :: Ord x => MapGraph x -> Graph.MapFunction x
successors mg l v = assert (v `Set.member` MapGraph.domain mg) $
  findWithDefault Set.empty (l,v) (successorMap mg)

predecessors :: Ord x => MapGraph x -> Graph.MapFunction x
predecessors mg l v = assert (v `Set.member` MapGraph.domain mg) $
  findWithDefault Set.empty (l,v) (predecessorMap mg)

fromGraph :: Ord a => Graph.GraphI g a -> g -> MapGraph a
fromGraph gi graph = 
  assert (Graph.wellDefined mapGraphI result) result where
    result = MapGraph dom sm pm
    dom = Graph.domain gi graph
    product = Set.cartesianProduct Graph.labels dom
    psucc = uncurry (Graph.successors gi graph)
    activeSuccDom = Set.filter (\p -> not (Set.null (psucc p))) product
    sm = Map.fromSet psucc activeSuccDom
    ppred = uncurry (Graph.predecessors gi graph)
    activePredDom = Set.filter (\p -> not (Set.null (ppred p))) product
    pm = Map.fromSet ppred activePredDom

subgraph :: Ord a => Graph.GraphI g a -> g -> Set a -> MapGraph a
subgraph gi g subdomain = assert (subdomain `isSubsetOf` Graph.domain gi g) $
  assert (Graph.wellDefined mapGraphI result) result where
    result = MapGraph dom sm pm
    dom = subdomain
    product = Set.cartesianProduct Graph.labels dom
    psucc (l,n) = Graph.successors gi g l n `Set.intersection` subdomain
    activeSuccDom = Set.filter (\p -> not (Set.null (psucc p))) product
    sm = Map.fromSet psucc activeSuccDom
    ppred (l,n) = Graph.predecessors gi g l n `Set.intersection` subdomain
    activePredDom = Set.filter (\p -> not (Set.null (ppred p))) product
    pm = Map.fromSet ppred activePredDom

projection :: (Ord a, Ord b) => Graph.GraphI g a -> g -> (a -> b) -> MapGraph b
projection gi g projection =
  assert (Graph.wellDefined mapGraphI result) result where
    result = MapGraph dom sm pm
    oldDomain = Graph.domain gi g
    dom = Set.map projection oldDomain
    product = Set.cartesianProduct Graph.labels dom
    preimage n = Set.filter (\m -> projection m == n) oldDomain
    mapper direction l = SetExtra.concatMap (Set.map projection . direction gi g l) . preimage
    psucc (l,n) = (mapper Graph.successors l) n
    activeSuccDom = Set.filter (\p -> not (Set.null (psucc p))) product
    sm = Map.fromSet psucc activeSuccDom
    ppred (l,n) = (mapper Graph.predecessors l) n
    activePredDom = Set.filter (\p -> not (Set.null (ppred p))) product
    pm = Map.fromSet ppred activePredDom

instance (Ord x, Show x) => Show (MapGraph x) where
  show = unlines . (Graph.prettyGraph mapGraphI show)
