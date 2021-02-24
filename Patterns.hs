module Patterns (
  triple,
  hamburger,
  force2d,
  force3d,
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

f2d Zero = [(Co,Co),(Co,OZ),(Co,OO)]
f2d One = [(OO,OO),(OO,OZ),(OZ,Co)]

force2d :: Graph N
force2d = assocToGraph f2d


f3d Zero = [('a','a'),('a','c'),('c','a'),('c','b')]
f3d One = [('a','c'),('b','a'),('b','b')]

force3d :: Graph Char
force3d = assocToGraph f3d
