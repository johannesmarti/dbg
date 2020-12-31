module MapGraph (
  MapGraph,
  fromGraph,
  toGraph,
) where

import Control.Exception.Base
import Data.Map.Strict as Map
import Data.Set as Set

import Graph

-- domains and codomains are stored as the keys that exists in the maps
data MapGraph x = MapGraph {
  domain         :: Set x,
  successorMap   :: Map (Label,x) (Set x),
  predecessorMap :: Map (Label,x) (Set x) 
}

successors :: Ord x => MapGraph x -> MapFunction x
successors mg l v = assert (v `Set.member` MapGraph.domain mg) $
  findWithDefault Set.empty (l,v) (successorMap mg)

predecessors :: Ord x => MapGraph x -> MapFunction x
predecessors mg l v = assert (v `Set.member` MapGraph.domain mg) $
  findWithDefault Set.empty (l,v) (predecessorMap mg)

toGraph :: Ord a => MapGraph a -> Graph a
toGraph mg = Graph.fromFunctions dom succ pred where
  dom = MapGraph.domain mg
  succ = MapGraph.successors mg
  pred = MapGraph.predecessors mg

fromGraph :: Ord a => Graph a -> MapGraph a
fromGraph graph = MapGraph dom sm pm where
  dom = Graph.domain graph
  product = Set.cartesianProduct labels dom
  psucc = uncurry (Graph.successors graph)
  activeSuccDom = Set.filter (\p -> not (Set.null (psucc p))) product
  sm = Map.fromSet psucc activeSuccDom
  ppred = uncurry (Graph.predecessors graph)
  activePredDom = Set.filter (\p -> not (Set.null (ppred p))) product
  pm = Map.fromSet ppred activePredDom
