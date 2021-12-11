module Patterns (
  triple,
  hamburger,
  force2d,
  force3d,
  allPaths,
  testPattern,
  strange3,
  strictDet,
  caleySchreck,
  caleySchreckSize,
  slowFourConcise,
  slowFourSize,
  slowFour,
  celtic,
  halfCeltic,
  slowSquare,
  deadEnd, deadEndI,
  deadEndWithoutEnd, deadEndWithoutEndI,
  slowLifting, slowLiftingI,
  goesWrong, goesWrongI,
) where

import qualified Data.Set as Set

import CommonLGraphTypes
import LabeledGraph
import Pretty
import BitGraph
import ConciseGraph

trap Zero = [('a','a'),('a','c'),('c','b'),('c','a')]
trap One = [('b','b'),('b','c'),('c','a'),('a','c')]
triple :: LMapGraph Char
triple = mapFromFunction trap

hb Zero = [('a','a'),('a','c'),('c','b'),('b','c'),('b','a'),('c','a')]
hb One = [('b','b'),('b','c'),('c','a'),('a','c'),('a','b'),('c','b')]
hamburger :: LMapGraph Char
hamburger = mapFromFunction hb

data N = Co | OZ | OO
  deriving (Eq,Ord,Show)
instance Pretty N where
  pretty n = show n

f2d Zero = [(Co,Co),(Co,OZ),(Co,OO)]
f2d One = [(OO,OO),(OO,OZ),(OZ,Co)]
force2d :: LMapGraph N
force2d = mapFromFunction f2d


f3d Zero = [('a','a'),('a','c'),('c','a'),('c','b')]
f3d One = [('a','c'),('b','a'),('b','b')]
force3d :: LMapGraph Char
force3d = mapFromFunction f3d

noPat1 Zero = [('a','a'),('a','c'),('c','b'),('b','a'),('b','c')]
noPat1 One = [('a','c'),('a','b'),('c','a'),('b','a'),('b','b')]
noPattern1 :: LMapGraph Char
noPattern1 = mapFromFunction noPat1

{- This pattern does not in fact have all paths. -}
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
allPaths :: LMapGraph Char
allPaths = mapFromFunction ap

test Zero = [('a','a'),('a','c'),('c','b'),('b','a'),('b','c')]
test One = [('a','c'),('a','b'),('c','a'),('b','a'),('b','b')]
testPattern :: LMapGraph Char
testPattern = mapFromFunction test

s3 Zero = [('a','a'),('a','b'),('b','a'),('b','c'),('c','b')]
s3 One = [('b','b'),('b','c'),('c','c'),('c','a')]
strange3 :: LMapGraph Char
strange3 = mapFromFunction s3

sd Zero = [('a','a'),('a','c'),('b','a'),('b','c'),('c','b')]
sd One = [('a','c'),('a','b'),('b','a'),('b','b'),('c','a'),('c','b')]
strictDet :: LMapGraph Char
strictDet = mapFromFunction sd

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
slowFour :: LMapGraph Char
slowFour = mapFromFunction s4

cel Zero = [('a','a'),('a','b'),
            ('b','b'),('b','c'),
            ('c','a'),('c','c')]
cel One  =  [('a','a'),('a','b'),
            ('b','b'),('b','c'),
            ('c','a'),('c','c')]
celtic :: LMapGraph Char
celtic = mapFromFunction cel

hcel Zero = [('a','a'),('a','b'),
            ('b','b'),('b','c'),
            ('c','a'),('c','c')]
hcel One = [('a','b'),('b','c'),('c','a')]
halfCeltic :: LMapGraph Char
halfCeltic = mapFromFunction hcel

-- 3569496551
slowSquare' Zero = [('a', 'a'), ('a', 'b'), ('a', 'c'), ('b', 'b'), ('b', 'c'), ('b', 'd'), ('c', 'a'), ('d', 'a'), ('d', 'b')]
slowSquare' One = [('a', 'b'), ('b', 'c'), ('b', 'd'), ('c', 'c'), ('d', 'a'), ('d', 'c'), ('d', 'd')]
slowSquare :: LMapGraph Char
slowSquare = mapFromFunction slowSquare'

deadEnd :: Word
deadEnd = 4072604
deadEndI :: LabeledGraphI Word Node
deadEndI = (conciseGraphI 4)

deadEndWithoutEnd :: LMapGraph Node
deadEndWithoutEnd = lMapSubgraphFromLGraph deadEndI deadEnd (Set.fromList [0,1,3])
deadEndWithoutEndI :: LabeledGraphI (LMapGraph Node) Node
deadEndWithoutEndI = lMapGraphI

{- Here the lifting converges quite slowly -}
slowLifting :: Word
slowLifting = 4966674
slowLiftingI :: LabeledGraphI Word Node
slowLiftingI = conciseGraphI 4

{- Here it seems where hard to find a homo -}
difficult :: Word
difficult = 2063974806
difficultI :: LabeledGraphI Word Node
difficultI = conciseGraphI 4

{- does not have path condition but is not construction deterministic -}
goesWrong :: Word
goesWrong = 1612382568
goesWrongI :: LabeledGraphI Word Node
goesWrongI = conciseGraphI 4
(conciseGraphI 4) 1612382568
(conciseGraphI 4) 1612382568
