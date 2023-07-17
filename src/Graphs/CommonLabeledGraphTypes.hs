{-# LANGUAGE FlexibleInstances #-}
module Graphs.CommonLabeledGraphTypes (
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

import qualified Graphs.GraphInterface as GI
import Graphs.BitGraph
import Graphs.LabeledGraphInterface
import Graphs.PairGraph as PG
import Graphs.AssocGraph
import Graphs.MapGraph as MG
import Graphs.PrettyNode
import Data.Label

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
assocFromFunction fct = PG.fromFunction (AssocGraph . fct)

mapFromFunction :: Ord x => (Label -> [(x,x)]) -> LabeledMapGraph x
mapFromFunction fct = fmap (MG.fromGraph assocGraphInterfaceNotPretty) $ assocFromFunction fct

labeledMapGraphFromLabeledGraph :: Ord x => LabeledGraphInterface g x -> g -> LabeledMapGraph x
labeledMapGraphFromLabeledGraph lgi g = PG.fromFunction f where
  f l = MG.fromGraph (graphIOfLabel lgi l) g

labeledMapSubgraphFromLabeledGraph :: Ord x => LabeledGraphInterface g x -> g -> Set x
                                            -> LabeledMapGraph x
labeledMapSubgraphFromLabeledGraph lgi g subdomain = PG.fromFunction f where
  f l = MG.subgraph (graphIOfLabel lgi l) g subdomain

labeledMapApplyBijection :: (Ord a, Ord b) => LabeledGraphInterface g a -> g -> (a -> b) -> LabeledMapGraph b
labeledMapApplyBijection gi g b = PG.fromFunction f where
  f l = MG.applyBijection (graphIOfLabel gi l) g b
 
labeledMapAddNodes :: Ord x => LabeledMapGraph x -> [x] -> LabeledMapGraph x
labeledMapAddNodes lmg nodes = PG.fromFunction f where
  f l = MG.addNodes (PG.graphOfLabel lmg l) nodes

labeledMapAddArcs :: Ord x => LabeledMapGraph x -> Label -> [(x,x)] -> LabeledMapGraph x
labeledMapAddArcs lmg label arcs = PG.fromFunction f where
  f l = if l == label
          then MG.addArcs (PG.graphOfLabel lmg l) arcs
          else PG.graphOfLabel lmg l

instance (Ord x, PrettyNode x) => Show (PairGraph (AssocGraph x)) where
  show g = unlines $ prettyLabeledGraph labeledAssocGraphInterface g

instance (Ord x, PrettyNode x) => Show (PairGraph (MapGraph x)) where
  show g = unlines $ prettyLabeledGraph labeledMapGraphInterface g
