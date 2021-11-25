module CommonGraphTypes (
  Label(..),
  LAssocGraph,
  LMapGraph,
  lAssocGraphI,
  lMapGraphI,
  assocFromFunction,
  mapFromFunction,
) where

import qualified Graph
import LabeledGraph
import PairGraph
import AssocGraph
import MapGraph
import Pretty

type LAssocGraph x = PairGraph (AssocGraph x)
type LMapGraph x = PairGraph (MapGraph x)

lAssocGraphI :: (Ord x, Pretty x) => LabeledGraphI (LAssocGraph x) x
lAssocGraphI = pairGraphI assocGraphI

lMapGraphI :: (Ord x, Pretty x) => LabeledGraphI (LMapGraph x) x
lMapGraphI = pairGraphI mapGraphI

assocFromFunction :: Ord x => (Label -> [(x,x)]) -> LAssocGraph x
assocFromFunction fct = PairGraph.fromFunction (AssocGraph . fct)

mapFromFunction :: Ord x => (Label -> [(x,x)]) -> LMapGraph x
mapFromFunction fct = fmap (MapGraph.fromGraph assocGraphINoShow) $ assocFromFunction fct

