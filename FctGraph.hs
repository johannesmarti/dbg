module FctGraph (
  FctGraph,
  fctGraphI, fctGraphINotPretty, fctGraphIwithNodePrinter,
  fctGraphFromDomain,
) where

import Control.Exception.Base
import qualified Data.Set as Set

import qualified Graph
import Pretty

data FctGraph x = FctGraph {
  domain :: Set.Set x,
  function :: x -> x -> Bool
}

fctGraphI :: (Ord x, Pretty x) => Graph.GraphI (FctGraph x) x
fctGraphI = fctGraphIwithNodePrinter pretty

fctGraphINotPretty :: Ord x => Graph.GraphI (FctGraph x) x
fctGraphINotPretty = fctGraphIwithNodePrinter (error "can not show nodes of this graph")

fctGraphIwithNodePrinter :: Ord x => (x -> String) -> Graph.GraphI (FctGraph x) x
fctGraphIwithNodePrinter prettyNode = Graph.interfaceFromHasArcPretty
					 domain
                                         hasAr
                                         (\_ n -> prettyNode n) where
  hasAr fg (x,y) = function fg x y

fctGraphFromDomain :: Ord x => Set.Set x -> (x -> x -> Bool) -> FctGraph x
fctGraphFromDomain dom fct = FctGraph dom fct

instance (Ord x, Pretty x) => Show (FctGraph x) where
  show = Graph.showG fctGraphI
