{-# LANGUAGE FlexibleInstances #-}
module CommonLabeledGraphTypes (
  LabeledAssocGraph,
  LabeledMapGraph,
  LabeledBitGraph,
  labeledAssocGraphInterface,
  labeledMapGraphInterface,
  labeledMapGraphInterfaceWithNodePrinter,
  labeledMapGraphInterfaceNotPretty,
  labeledBitGraphInterface,
  assocFromFunction,
  mapFromFunction,
  labeledMapGraphFromLabeledGraph,
  labeledMapSubgraphFromLabeledGraph,
  labeledMapApplyBijection, 
  labeledMapAddNodes,
  labeledMapAddArcs,
) where

import Data.Set

import qualified GraphInterface as GI
import BitGraph
import LabeledGraphInterface
import PairGraph
import AssocGraph
import MapGraph
import PrettyNode

type LabeledAssocGraph x = PairGraph (AssocGraph x)
type LabeledMapGraph x = PairGraph (MapGraph x)
type LabeledBitGraph = PairGraph BitGraph

graphIOfLabel :: LabeledGraphInterface g x -> Label -> GI.GraphInterface g x
graphIOfLabel lgi l = GI.interfaceFromAll (domain lgi) (\g -> successors lgi g l) (\g -> predecessors lgi g l) (\g -> hasArc lgi g l) (\g -> arcsOfLabel lgi g l) (prettyNode lgi)

labeledAssocGraphInterface :: (Ord x, PrettyNode x) => LabeledGraphInterface (LabeledAssocGraph x) x
labeledAssocGraphInterface = pairGraphInterface assocGraphInterface

labeledMapGraphInterface :: (Ord x, PrettyNode x) => LabeledGraphInterface (LabeledMapGraph x) x
labeledMapGraphInterface = pairGraphInterface mapGraphInterface

labeledMapGraphInterfaceWithNodePrinter :: Ord x => (x -> String) -> LabeledGraphInterface (LabeledMapGraph x) x
labeledMapGraphInterfaceWithNodePrinter printer = pairGraphInterface (mapGraphInterfaceWithNodePrinter printer)

labeledMapGraphInterfaceNotPretty :: Ord x => LabeledGraphInterface (LabeledMapGraph x) x
labeledMapGraphInterfaceNotPretty = pairGraphInterface mapGraphInterfaceNotPretty

labeledBitGraphInterface :: Size -> LabeledGraphInterface (LabeledBitGraph) Node
labeledBitGraphInterface size = pairGraphInterface (bitGraphInterface size)

assocFromFunction :: Ord x => (Label -> [(x,x)]) -> LabeledAssocGraph x
assocFromFunction fct = PairGraph.fromFunction (AssocGraph . fct)

mapFromFunction :: Ord x => (Label -> [(x,x)]) -> LabeledMapGraph x
mapFromFunction fct = fmap (MapGraph.fromGraph assocGraphInterfaceNotPretty) $ assocFromFunction fct

labeledMapGraphFromLabeledGraph :: Ord x => LabeledGraphInterface g x -> g -> LabeledMapGraph x
labeledMapGraphFromLabeledGraph lgi g = PairGraph.fromFunction f where
  f l = MapGraph.fromGraph (graphIOfLabel lgi l) g

labeledMapSubgraphFromLabeledGraph :: Ord x => LabeledGraphInterface g x -> g -> Set x
                                            -> LabeledMapGraph x
labeledMapSubgraphFromLabeledGraph lgi g subdomain = PairGraph.fromFunction f where
  f l = MapGraph.subgraph (graphIOfLabel lgi l) g subdomain

labeledMapApplyBijection :: (Ord a, Ord b) => LabeledGraphInterface g a -> g -> (a -> b) -> LabeledMapGraph b
labeledMapApplyBijection gi g b = PairGraph.fromFunction f where
  f l = MapGraph.applyBijection (graphIOfLabel gi l) g b
 
labeledMapAddNodes :: Ord x => LabeledMapGraph x -> [x] -> LabeledMapGraph x
labeledMapAddNodes lmg nodes = PairGraph.fromFunction f where
  f l = MapGraph.addNodes (PairGraph.graphOfLabel lmg l) nodes

labeledMapAddArcs :: Ord x => LabeledMapGraph x -> Label -> [(x,x)] -> LabeledMapGraph x
labeledMapAddArcs lmg label arcs = PairGraph.fromFunction f where
  f l = if l == label
          then MapGraph.addArcs (PairGraph.graphOfLabel lmg l) arcs
          else PairGraph.graphOfLabel lmg l

instance (Ord x, PrettyNode x) => Show (PairGraph (AssocGraph x)) where
  show g = unlines $ prettyLabeledGraph labeledAssocGraphInterface g

instance (Ord x, PrettyNode x) => Show (PairGraph (MapGraph x)) where
  show g = unlines $ prettyLabeledGraph labeledMapGraphInterface g
