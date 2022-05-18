module FunctionGraph (
  FunctionGraph,
  functionGraphI,
  functionGraphIWithNodePrinter,
  fromDomSuccPred,
) where

import Control.Exception.Base
import qualified Data.Set as Set

import qualified Graph as Graph
import Pretty

data FunctionGraph x = FunctionGraph {
  domain :: Set.Set x,
  successors :: x -> Set.Set x,
  predecessors :: x -> Set.Set x
}

functionGraphI :: (Ord x,Pretty x) => Graph.GraphI (FunctionGraph x) x
functionGraphI = functionGraphIWithNodePrinter pretty

functionGraphINotPretty :: Ord x => Graph.GraphI (FunctionGraph x) x
functionGraphINotPretty = functionGraphIWithNodePrinter (error "can not show nodes of this graph")

functionGraphIWithNodePrinter :: Ord x => (x -> String) -> Graph.GraphI (FunctionGraph x) x
functionGraphIWithNodePrinter prettyNode = Graph.interfaceFromSuccPredPretty
                                             domain successors predecessors
                                             (\_ n -> prettyNode n) 

fromDomSuccPred :: Ord x => Set.Set x -> (x -> Set.Set x) -> (x -> Set.Set x) -> FunctionGraph x
fromDomSuccPred dom succ pred = 
  assert (Graph.succPredInDom functionGraphINotPretty result) $
  assert (Graph.succPredMatch functionGraphINotPretty result) result where
    result = FunctionGraph dom succ pred
