module Patterns (
  triple,
  hamburger,
  force2,
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


hb Zero = [('a','a'),('a','c'),('c','b'),('b','c'),('b','a'),('c','a')]
hb One = [('b','b'),('b','c'),('c','a'),('a','c'),('a','b'),('c','b')]

hamburger :: Graph Char
hamburger = assocToGraph hb


data N = Co | OZ | OO
  deriving (Eq,Ord,Show)

f2 Zero = [(Co,Co),(Co,OZ),(Co,OO)]
f2 One = [(OO,OO),(OO,OZ),(OZ,Co)]

force2 :: Graph N
force2 = assocToGraph f2
