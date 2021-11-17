module Patterns (
  triple,
  hamburger,
  force2d,
  force3d,
  allPaths,
  testPattern,
  strange3,
  strongDet,
  caleySchreck,
  caleySchreckSize,
  slowFourConcise,
  slowFourSize,
  slowFour,
  celtic,
  slowSquare,
  diverger3,
  diverger3Size,
) where

import AssocGraph
import ConciseGraph
import Graph
import MapGraph

trap Zero = [('a','a'),('a','c'),('c','b'),('c','a')]
trap One = [('b','b'),('b','c'),('c','a'),('a','c')]
triple :: MapGraph Char
triple = assocToMap trap

hb Zero = [('a','a'),('a','c'),('c','b'),('b','c'),('b','a'),('c','a')]
hb One = [('b','b'),('b','c'),('c','a'),('a','c'),('a','b'),('c','b')]
hamburger :: MapGraph Char
hamburger = assocToMap hb

data N = Co | OZ | OO
  deriving (Eq,Ord,Show)
f2d Zero = [(Co,Co),(Co,OZ),(Co,OO)]
f2d One = [(OO,OO),(OO,OZ),(OZ,Co)]
force2d :: MapGraph N
force2d = assocToMap f2d


f3d Zero = [('a','a'),('a','c'),('c','a'),('c','b')]
f3d One = [('a','c'),('b','a'),('b','b')]
force3d :: MapGraph Char
force3d = assocToMap f3d

noPat1 Zero = [('a','a'),('a','c'),('c','b'),('b','a'),('b','c')]
noPat1 One = [('a','c'),('a','b'),('c','a'),('b','a'),('b','b')]
noPattern1 :: MapGraph Char
noPattern1 = assocToMap noPat1

u = 'u'
v = 'v'
w = 'w'
x = 'x'
a = 'a'
b = 'b'
c = 'c'
d = 'd'
ap Zero = [(u,v),(u,w),(u,x),(v,u),(v,w),(v,x)
          ,(w,u),(w,v),(w,w),(w,x),(x,u),(x,v),(x,w)
          ,(u,a),(u,b),(w,d),(x,c)]
ap One = [(a,a),(a,b),(a,c),(a,d),(b,a),(b,c),(b,d)
          ,(c,a),(c,b),(c,d),(d,a),(d,b),(d,c)
          ,(a,u),(b,v),(c,w),(d,x)]
allPaths :: MapGraph Char
allPaths = assocToMap ap

test Zero = [('a','a'),('a','c'),('c','b'),('b','a'),('b','c')]
test One = [('a','c'),('a','b'),('c','a'),('b','a'),('b','b')]
testPattern :: MapGraph Char
testPattern = assocToMap test

s3 Zero = [('a','a'),('a','b'),('b','a'),('b','c'),('c','b')]
s3 One = [('b','b'),('b','c'),('c','c'),('c','a')]
strange3 :: MapGraph Char
strange3 = assocToMap s3

sd Zero = [('a','a'),('a','c'),('b','a'),('b','c'),('c','b')]
sd One = [('a','c'),('a','b'),('b','a'),('b','b'),('c','a'),('c','b')]
strongDet :: MapGraph Char
strongDet = assocToMap sd

caleySchreck :: ConciseGraph
caleySchreck = 3942849
caleySchreckSize :: Size
caleySchreckSize = 4

slowFourConcise :: ConciseGraph
slowFourConcise = 4003476
slowFourSize :: Size
slowFourSize = 4
s4 Zero = [('a','a'),('a','b'),('b','c'),('b','d'),('c','a'),('d','c')]
s4 One = [('b','b'),('b','c'),('c','a'),('c','c'),('c','d')]
slowFour :: MapGraph Char
slowFour = assocToMap s4

cel Zero = [('a','a'),('a','b'),
            ('b','b'),('b','c'),
            ('c','a'),('c','c')]
cel One = [('a','b'),('b','c'),('c','a')]
celtic :: MapGraph Char
celtic = assocToMap cel

slowSquare' Zero = [('a', 'a'), ('a', 'b'), ('a', 'c'), ('b', 'b'), ('b', 'c'), ('b', 'd'), ('c', 'a'), ('d', 'a'), ('d', 'b')]
slowSquare' One = [('a', 'b'), ('b', 'c'), ('b', 'd'), ('c', 'c'), ('d', 'a'), ('d', 'c'), ('d', 'd')]
slowSquare :: MapGraph Char
slowSquare = assocToMap slowSquare'

diverger3 :: ConciseGraph
diverger3 = 44199
diverger3Size :: Size
diverger3Size = 3
