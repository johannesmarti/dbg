module Patterns (
  triple,
  hamburger,
  force2d,
  force3d,
  testPattern,
) where

import AssocGraph
import Graph
import MapGraph

assocToGraph :: (Ord a, Show a) => AssocGraph a -> Graph a
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


noPat1 Zero = [('a','a'),('a','c'),('c','b'),('b','a'),('b','c')]
noPat1 One = [('a','c'),('a','b'),('c','a'),('b','a'),('b','b')]

noPattern1 :: Graph Char
noPattern1 = assocToGraph noPat1


test Zero = [('a','a'),('a','c'),('c','b'),('b','a'),('b','c')]
test One = [('a','c'),('a','b'),('c','a'),('b','a'),('b','b')]

testPattern :: Graph Char
testPattern = assocToGraph test

