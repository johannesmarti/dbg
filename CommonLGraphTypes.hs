{-# LANGUAGE FlexibleInstances #-}
module CommonLGraphTypes (
  Label(..),
  LAssocGraph,
  LMapGraph,
  LBitGraph,
  lAssocGraphI,
  lMapGraphI,
  lBitGraphI,
  assocFromFunction,
  mapFromFunction,
  caleyGraphOfLBitGraph,
) where

import qualified Graph
import BitGraph
import LabeledGraph
import PairGraph
import AssocGraph
import MapGraph
import CaleyGraph
import Pretty

type LAssocGraph x = PairGraph (AssocGraph x)
type LMapGraph x = PairGraph (MapGraph x)
type LBitGraph = PairGraph BitGraph

lAssocGraphI :: (Ord x, Pretty x) => LabeledGraphI (LAssocGraph x) x
lAssocGraphI = pairGraphI assocGraphI

lMapGraphI :: (Ord x, Pretty x) => LabeledGraphI (LMapGraph x) x
lMapGraphI = pairGraphI mapGraphI

lBitGraphI :: Size -> LabeledGraphI (LBitGraph) Node
lBitGraphI size = pairGraphI (bitGraphI size)

assocFromFunction :: Ord x => (Label -> [(x,x)]) -> LAssocGraph x
assocFromFunction fct = PairGraph.fromFunction (AssocGraph . fct)

mapFromFunction :: Ord x => (Label -> [(x,x)]) -> LMapGraph x
mapFromFunction fct = fmap (MapGraph.fromGraph assocGraphINoShow) $ assocFromFunction fct

caleyGraphOfLBitGraph :: Size -> LBitGraph -> CaleyGraph
caleyGraphOfLBitGraph size bg = rightCaleyGraph size
                                  (graphOfLabel bg Zero, graphOfLabel bg One)

instance (Ord x, Pretty x) => Show (PairGraph (AssocGraph x)) where
  show g = unlines $ prettyLabeledGraph lAssocGraphI g

instance (Ord x, Pretty x) => Show (PairGraph (MapGraph x)) where
  show g = unlines $ prettyLabeledGraph lMapGraphI g
