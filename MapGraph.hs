module MapGraph (
  MapGraph,
  mapGraphI,
  fromGraph,
) where

import Control.Exception.Base
import Data.Map.Strict as Map
import Data.Set as Set

import qualified Graph

-- TODO: Maybe we should but the Label into the codomain by mapping xs to pairs of sets
data MapGraph x = MapGraph {
  domain         :: Set x,
  successorMap   :: Map (Graph.Label,x) (Set x),
  predecessorMap :: Map (Graph.Label,x) (Set x) 
}

mapGraphI :: Ord x => Graph.GraphI (MapGraph x) x
mapGraphI = Graph.GraphI domain successors predecessors

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

instance (Ord x, Show x) => Show (MapGraph x) where
  show = unlines . (Graph.prettyGraph mapGraphI show)
