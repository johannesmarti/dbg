{-# LANGUAGE FlexibleInstances #-}
module CommonLGraphTypes (
  Label(..),
  LAssocGraph,
  LMapGraph,
  LBitGraph,
  lAssocGraphI,
  lMapGraphI,
  lMapGraphIWithNodePrinter,
  lMapGraphINotPretty,
  lBitGraphI,
  assocFromFunction,
  mapFromFunction,
  lMapGraphFromLGraph,
  lMapSubgraphFromLGraph,
  lMapApplyBijection, 
  lMapAddNodes,
  lMapAddArcs,
  cayleyGraphOfLBitGraph,
) where

import Data.Set

import qualified Graph
import BitGraph
import LabeledGraph
import PairGraph
import AssocGraph
import MapGraph
import CayleyGraph hiding (domain)
import Pretty

type LAssocGraph x = PairGraph (AssocGraph x)
type LMapGraph x = PairGraph (MapGraph x)
type LBitGraph = PairGraph BitGraph

graphOfLabelI :: LabeledGraphI g x -> Label -> Graph.GraphI g x
graphOfLabelI lgi l = Graph.interfaceFromAll (domain lgi) (\g -> successors lgi g l) (\g -> predecessors lgi g l) (\g -> hasArc lgi g l) (\g -> arcsOfLabel lgi g l) (prettyNode lgi)

lAssocGraphI :: (Ord x, Pretty x) => LabeledGraphI (LAssocGraph x) x
lAssocGraphI = pairGraphI assocGraphI

lMapGraphI :: (Ord x, Pretty x) => LabeledGraphI (LMapGraph x) x
lMapGraphI = pairGraphI mapGraphI

lMapGraphIWithNodePrinter :: Ord x => (x -> String) -> LabeledGraphI (LMapGraph x) x
lMapGraphIWithNodePrinter printer = pairGraphI (mapGraphIWithNodePrinter printer)

lMapGraphINotPretty :: Ord x => LabeledGraphI (LMapGraph x) x
lMapGraphINotPretty = pairGraphI mapGraphINotPretty

lBitGraphI :: Size -> LabeledGraphI (LBitGraph) Node
lBitGraphI size = pairGraphI (bitGraphI size)

assocFromFunction :: Ord x => (Label -> [(x,x)]) -> LAssocGraph x
assocFromFunction fct = PairGraph.fromFunction (AssocGraph . fct)

mapFromFunction :: Ord x => (Label -> [(x,x)]) -> LMapGraph x
mapFromFunction fct = fmap (MapGraph.fromGraph assocGraphINotPretty) $ assocFromFunction fct

lMapGraphFromLGraph :: Ord x => LabeledGraphI g x -> g -> LMapGraph x
lMapGraphFromLGraph lgi g = PairGraph.fromFunction f where
  f l = MapGraph.fromGraph (graphOfLabelI lgi l) g

lMapSubgraphFromLGraph :: Ord x => LabeledGraphI g x -> g -> Set x
                                   -> LMapGraph x
lMapSubgraphFromLGraph lgi g subdomain = PairGraph.fromFunction f where
  f l = MapGraph.subgraph (graphOfLabelI lgi l) g subdomain

lMapApplyBijection :: (Ord a, Ord b) => LabeledGraphI g a -> g -> (a -> b) -> LMapGraph b
lMapApplyBijection gi g b = PairGraph.fromFunction f where
  f l = MapGraph.applyBijection (graphOfLabelI gi l) g b
 
lMapAddNodes :: Ord x => LMapGraph x -> [x] -> LMapGraph x
lMapAddNodes lmg nodes = PairGraph.fromFunction f where
  f l = MapGraph.addNodes (PairGraph.graphOfLabel lmg l) nodes

lMapAddArcs :: Ord x => LMapGraph x -> Label -> [(x,x)] -> LMapGraph x
lMapAddArcs lmg label arcs = PairGraph.fromFunction f where
  f l = if l == label
          then MapGraph.addArcs (PairGraph.graphOfLabel lmg l) arcs
          else PairGraph.graphOfLabel lmg l

cayleyGraphOfLBitGraph :: Size -> LBitGraph -> CayleyGraph
cayleyGraphOfLBitGraph size bg = rightCayleyGraph size
                                  (graphOfLabel bg Zero, graphOfLabel bg One)

instance (Ord x, Pretty x) => Show (PairGraph (AssocGraph x)) where
  show g = unlines $ prettyLabeledGraph lAssocGraphI g

instance (Ord x, Pretty x) => Show (PairGraph (MapGraph x)) where
  show g = unlines $ prettyLabeledGraph lMapGraphI g
