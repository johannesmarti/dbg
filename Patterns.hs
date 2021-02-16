module Patterns (
  triple,
) where

import AssocGraph
import Graph
import MapGraph

assocToGraph :: Ord a => AssocGraph a -> Graph a
assocToGraph = MapGraph.toGraph . MapGraph.fromGraph . AssocGraph.toGraph

trap Zero = [('a','a'),('a','c'),('c','b'),('c','a')]
trap One = [('b','b'),('b','c'),('c','a'),('a','c')]

triple :: Graph Char
triple = assocToGraph trap
