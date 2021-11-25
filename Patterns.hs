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
  totalIrreflexive
) where

import AssocGraph
import MapGraph
import PairGraph
--import ConciseGraph
import qualified Graph
import LabeledGraph
import Pretty

type ConciseGraph = Word
type Size = Int

type LMapGraph x = PairGraph (MapGraph x)
type LMapGraphChar = LMapGraph Char

lMapGraphI :: (Ord x, Pretty x) => LabeledGraphI (LMapGraph x) x
lMapGraphI = pairGraphI mapGraphI

assocToMap :: Ord x => (Label -> [(x,x)]) -> LMapGraph x
assocToMap fct = fmap (MapGraph.fromGraph assocGraphINoShow) $ PairGraph.fromFunction (AssocGraph . fct)

trap Zero = [('a','a'),('a','c'),('c','b'),('c','a')]
trap One = [('b','b'),('b','c'),('c','a'),('a','c')]
triple :: LMapGraphChar
triple = assocToMap trap

hb Zero = [('a','a'),('a','c'),('c','b'),('b','c'),('b','a'),('c','a')]
hb One = [('b','b'),('b','c'),('c','a'),('a','c'),('a','b'),('c','b')]
hamburger :: LMapGraphChar
hamburger = assocToMap hb

data N = Co | OZ | OO
  deriving (Eq,Ord,Show)
f2d Zero = [(Co,Co),(Co,OZ),(Co,OO)]
f2d One = [(OO,OO),(OO,OZ),(OZ,Co)]
force2d :: LMapGraph N
force2d = assocToMap f2d


f3d Zero = [('a','a'),('a','c'),('c','a'),('c','b')]
f3d One = [('a','c'),('b','a'),('b','b')]
force3d :: LMapGraphChar
force3d = assocToMap f3d

noPat1 Zero = [('a','a'),('a','c'),('c','b'),('b','a'),('b','c')]
noPat1 One = [('a','c'),('a','b'),('c','a'),('b','a'),('b','b')]
noPattern1 :: LMapGraphChar
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
allPaths :: LMapGraphChar
allPaths = assocToMap ap

test Zero = [('a','a'),('a','c'),('c','b'),('b','a'),('b','c')]
test One = [('a','c'),('a','b'),('c','a'),('b','a'),('b','b')]
testPattern :: LMapGraphChar
testPattern = assocToMap test

s3 Zero = [('a','a'),('a','b'),('b','a'),('b','c'),('c','b')]
s3 One = [('b','b'),('b','c'),('c','c'),('c','a')]
strange3 :: LMapGraphChar
strange3 = assocToMap s3

sd Zero = [('a','a'),('a','c'),('b','a'),('b','c'),('c','b')]
sd One = [('a','c'),('a','b'),('b','a'),('b','b'),('c','a'),('c','b')]
strongDet :: LMapGraphChar
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
slowFour :: LMapGraphChar
slowFour = assocToMap s4

cel Zero = [('a','a'),('a','b'),
            ('b','b'),('b','c'),
            ('c','a'),('c','c')]
cel One = [('a','b'),('b','c'),('c','a')]
celtic :: LMapGraphChar
celtic = assocToMap cel

slowSquare' Zero = [('a', 'a'), ('a', 'b'), ('a', 'c'), ('b', 'b'), ('b', 'c'), ('b', 'd'), ('c', 'a'), ('d', 'a'), ('d', 'b')]
slowSquare' One = [('a', 'b'), ('b', 'c'), ('b', 'd'), ('c', 'c'), ('d', 'a'), ('d', 'c'), ('d', 'd')]
slowSquare :: LMapGraphChar
slowSquare = assocToMap slowSquare'

diverger3 :: ConciseGraph
diverger3 = 44199
diverger3Size :: Size
diverger3Size = 3

ti Zero = [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
ti One = [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
totalIrreflexive :: LMapGraph Int
totalIrreflexive = assocToMap ti

