module Patterns (
  triple,
  hamburger,
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

hb Zero = [('a','a'),('a','c'),('c','b'),('b','c'),('b','a')]
hb One = [('b','b'),('b','c'),('c','a'),('a','c'),('a','b')]

hamburger :: Graph Char
hamburger = assocToGraph hb
