module Patterns (
  triple,
  tripleM,
) where

import AssocGraph
import Graph
import MapGraph

trap Zero = [('a','a'),('a','c'),('c','b'),('c','a')]
trap One = [('b','b'),('b','c'),('c','a'),('a','c')]

triple :: Graph Char
triple = AssocGraph.toGraph trap

tripleM :: Graph Char
tripleM = MapGraph.toGraph . MapGraph.fromGraph . AssocGraph.toGraph $ trap
