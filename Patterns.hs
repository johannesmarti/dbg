module Patterns (
  triple,
  hamburger, hamburgerI,
  force2d,
  force3d, force3dI,
  allPaths,
  testPattern,
  strange3,
  strictDet,
  cayleySchreck,
  cayleySchreckSize,
  slowFourConcise, slowFourConciseI, slowFourConciseSize,
  celtic,
  halfCeltic,
  slowSquare, slowSquareI,
  difficult, difficultI,
  deadEnd, deadEndI,
  deadEndWithoutEnd, deadEndWithoutEndI,
  slowLifting, slowLiftingI,
  goesWrong, goesWrongI,
  complicatedPos,
  complicatedNeg,
  unsound, unsoundI,
  noPath, noPathI,
  notQuitePath, notQuitePathI,
  zoComp, zoCompI,
  ex1, ex1I,
  force4d, force4dI,
  force4d',
  force5d, force5dI,
  force5d',
  ex2, ex2I,
  ex3, ex3I,
  ex4, ex4I,
  ex5, ex5I,
  uh, uhI,
  study, studyI,
  growingLifting, growingLiftingI,
  biggest, biggestI,
  alsoBig, alsoBigI,
  big5, big5I,
  force6d, force6dI,
  force7d, force7dI,
  force8d, force8dI,
  force9d, force9dI,
  alloc1, alloc1I,
  alloc2, alloc2I,
  alloc3, alloc3I,
  crazy, crazyI,
  crazier, crazierI,
  issues, issuesI,
  unfolded, unfoldedI,
) where

import qualified Data.Set as Set

import CommonLGraphTypes
import LabeledGraph
import Pretty
import BitGraph
import ConciseGraph
import qualified PairGraph

trap Zero = [('a','a'),('a','c'),('c','b'),('c','a')]
trap One = [('b','b'),('b','c'),('c','a'),('a','c')]
triple :: LMapGraph Char
triple = mapFromFunction trap

hb Zero = [('a','a'),('a','c'),('c','b'),('b','c'),('b','a'),('c','a')]
hb One = [('b','b'),('b','c'),('c','a'),('a','c'),('a','b'),('c','b')]
hamburger :: LMapGraph Char
hamburger = mapFromFunction hb
hamburgerI :: LabeledGraphI (LMapGraph Char) Char
hamburgerI = lMapGraphI

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
force3dI :: LabeledGraphI (LMapGraph Char) Char
force3dI = lMapGraphI

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

cayleySchreck :: ConciseGraph
cayleySchreck = fromOldCode 3942849
cayleySchreckSize :: Size
cayleySchreckSize = 4

{- This pattern is construction deterministic -}
slowFourConcise :: ConciseGraph
slowFourConcise = fromOldCode 4003476
slowFourConciseSize :: Size
slowFourConciseSize = 4
slowFourConciseI :: LabeledGraphI ConciseGraph Node
slowFourConciseI = conciseGraphI slowFourConciseSize

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
slowSquareI :: LabeledGraphI (LMapGraph Char) Char
slowSquareI = lMapGraphI

deadEnd :: ConciseGraph
deadEnd = fromOldCode 4072604
deadEndI :: LabeledGraphI ConciseGraph Node
deadEndI = (conciseGraphI 4)

deadEndWithoutEnd :: LMapGraph Node
deadEndWithoutEnd = lMapSubgraphFromLGraph deadEndI deadEnd (Set.fromList [0,1,3])
deadEndWithoutEndI :: LabeledGraphI (LMapGraph Node) Node
deadEndWithoutEndI = lMapGraphI

{- is not construction deterministic. -}
slowLifting :: ConciseGraph 
slowLifting = fromOldCode 4966674
slowLiftingI :: LabeledGraphI ConciseGraph Node
slowLiftingI = conciseGraphI 4

{- Here it seems where hard to find a homo -}
difficult :: ConciseGraph
difficult = fromOldCode 2063974806
difficultI :: LabeledGraphI ConciseGraph Node
difficultI = conciseGraphI 4

{- does not have path condition but is not construction deterministic -}
goesWrong :: ConciseGraph
goesWrong = fromOldCode 1612382568
goesWrongI :: LabeledGraphI ConciseGraph Node
goesWrongI = conciseGraphI 4

-- complicated
complicatedPos' Zero = [('a', 'a'), ('a', 'c'), ('c', 'a'), ('c', 'b'), ('b', 'a'), ('b', 'c')]
complicatedPos' One = [('a', 'c'), ('a', 'b'), ('c', 'a'), ('c', 'b'), ('b', 'a'), ('b', 'b')]
complicatedPos :: LMapGraph Char
complicatedPos = mapFromFunction complicatedPos'

-- complicated
complicatedNeg' Zero = [('a', 'a'), ('a', 'c'), ('c', 'a'), ('c', 'b'), ('b', 'a'), ('b', 'c')]
complicatedNeg' One = [('a', 'c'), ('a', 'b'), ('c', 'a'), ('c', 'b'), ('b', 'c'), ('b', 'b')]
complicatedNeg :: LMapGraph Char
complicatedNeg = mapFromFunction complicatedNeg'

{- This shows that the unsound filter for the lifting is indeed unsound. -}
unsound :: ConciseGraph
unsound = fromOldCode 2063931814
unsoundI :: LabeledGraphI ConciseGraph Node
unsoundI = conciseGraphI 4

{- The following pattern is not construction deterministic but also does not satisfy the path condition -}
noPath :: ConciseGraph
noPath = fromOldCode 988302
noPathI :: LabeledGraphI ConciseGraph Node
noPathI = conciseGraphI 4

{- This pattern satisfies the path condition up to words of length 2, but not for longer words. It is construction deterministic. -}
notQuitePath :: ConciseGraph
notQuitePath = fromOldCode 57450828
notQuitePathI :: LabeledGraphI ConciseGraph Node
notQuitePathI = conciseGraphI 4

{- In this pattern there is a 0-loop that reaches all other nodes over a 0-path and similarly for 1. But it does not satify the path condition. It is not construction deterministic. -}
zoComp :: ConciseGraph
zoComp = fromOldCode 23617753
zoCompI :: LabeledGraphI ConciseGraph Node
zoCompI = conciseGraphI 4

{- just a random example -}
e1 Zero = [('a','a'),('a','x'),('x','u'),('u','v'),('v','x'),('a','y'),('y','b')]
e1 One = [('b','b'),('b','a'),('b','x'),('b','u'),('b','v'),('x','y')]
ex1 :: LMapGraph Char
ex1 = mapFromFunction e1
ex1I :: LabeledGraphI (LMapGraph Char) Char
ex1I = lMapGraphI

{- Just some pattern with a homo at 4. -}
force4d :: ConciseGraph
force4d = fromOldCode 201822290
force4dI :: LabeledGraphI ConciseGraph Node
force4dI = conciseGraphI 4

force4d' :: ConciseGraph
force4d' = ConciseGraph.fromLBitGraph 4 (PairGraph.fromFunction fct) where
  fct Zero = PairGraph.graphOfLabel pairGraph Zero
  fct One  = BitGraph.delArc 4 (PairGraph.graphOfLabel pairGraph One) (2,2)
  pairGraph = ConciseGraph.toLBitGraph 4 force4d

{- Just some pattern with a homo at 5. -}
force5d :: ConciseGraph
force5d = fromOldCode 201822534
force5dI :: LabeledGraphI ConciseGraph Node
force5dI = conciseGraphI 4

force5d' :: ConciseGraph
force5d' = ConciseGraph.fromLBitGraph 4 (PairGraph.fromFunction fct) where
  fct Zero = PairGraph.graphOfLabel pairGraph Zero
  fct One  = BitGraph.delArc 4 (BitGraph.delArc 4 (PairGraph.graphOfLabel pairGraph One) (2,2)) (2,2)
  pairGraph = ConciseGraph.toLBitGraph 4 force5d

{- another random example -}
e2 Zero = [('a','a'),('a','o'),('a','x'),('a','z'),('o','u'),('x','v'),('x','i'),('z','b'),('u','y'),('u','v')]
e2 One = [('b','b'),('b','i'),('b','y'),('b','a'),('i','v'),('y','z'),('y','o'),('y','u'),('y','x'),('v','u')]
ex2 :: LMapGraph Char
ex2 = mapFromFunction e2
ex2I :: LabeledGraphI (LMapGraph Char) Char
ex2I = lMapGraphI

{- An example in which initially all nodes are 1 00-step away from a but then in the first lifting [y b] is only reachable via a 000-path. -}
e3 Zero = [('a','a'),('a','x'),('a','y'),('x','u'),('x','b'),('u','y'),('u','b')]
e3 One = [('b','b'),('b','y'),('b','u'),('b','a'),('y','x')]
ex3 :: LMapGraph Char
ex3 = mapFromFunction e3
ex3I :: LabeledGraphI (LMapGraph Char) Char
ex3I = lMapGraphI

{- One more example to play around with -}
e4 Zero = [('a','a'),('a','x'),('a','u'),('x','b'),('x','y'),('u','v')]
e4 One = [('b','b'),('b','y'),('b','v'),('y','a'),('v','x'),('v','u')]
ex4 :: LMapGraph Char
ex4 = mapFromFunction e4
ex4I :: LabeledGraphI (LMapGraph Char) Char
ex4I = lMapGraphI

{- It satisfies the path condition and is not construction deterministic. -}
e5 Zero =
  [("a","a"),("a","a'"),("a","b'"),("a","x"),
   ("a'","b"),("a'","u"),("a'","y"),("a'","y'"),("a'","v"),("a'","x'"),
   ("u","v"),("u","y"),("u","y'"),("u","a"),("u","a'"),
   ("x","x'"), ("x","b"),
   ("x'","x"), ("x'","b'")]
e5 One =
  [("b","b"),("b","b'"),("b","a"),("b","y"),
   ("b'","a'"),("b'","v"),("b'","x"),("b'","x'"),("b'","u"),("b'","y'"),
   ("v","u"),("v","x"),("v","x'"),("v","b"),("v","b'"),
   ("y","y'"), ("y","a"),
   ("y'","y"), ("y'","a'")]
ex5 :: LMapGraph String
ex5 = mapFromFunction e5
ex5I :: LabeledGraphI (LMapGraph String) String
ex5I = lMapGraphI

{- It satisfies the path condition and is not construction deterministic. -}
uhf Zero =
  [("a","a"),("a","c'"),("a","a'"),("a","c''"),("a","c'''"),
   ("c'","b"),
   ("a'","c'''")]
uhf One =
  [("b","b"),("b","c''"),("b","c'"),("b","c'''"),
   ("c''","a"),("c''","a'"),
   ("c'''","a'")]
uh :: LMapGraph String
uh = mapFromFunction uhf
uhI :: LabeledGraphI (LMapGraph String) String
uhI = lMapGraphI

stud Zero = [('b','a'),('c','a'),('c','b'),('c','c')]
stud One = [('a','a'),('a','b'),('b','c')]
study :: LMapGraph Char
study = mapFromFunction stud
studyI :: LabeledGraphI (LMapGraph Char) Char
studyI = lMapGraphI

sld = ['a', 'b', 'c']
sl Zero = [('a','a'),('a','b'),('b','c'),('c','b'),('c','c')]
sl One = [(x,y) | x <- sld, y <- sld] 
growingLifting :: LMapGraph Char
growingLifting = mapFromFunction sl
growingLiftingI :: LabeledGraphI (LMapGraph Char) Char
growingLiftingI = lMapGraphI

{- This pattern is the pattern such that it has a homo for a maximal dimension among all the patterns of size 4 -}
biggest :: ConciseGraph
biggest = fromOldCode 2458141589
biggestI :: LabeledGraphI ConciseGraph Node
biggestI = conciseGraphI 4

alsoBig :: ConciseGraph
alsoBig = fromOldCode 675781230
alsoBigI :: LabeledGraphI ConciseGraph Node
alsoBigI = conciseGraphI 4

big5 :: ConciseGraph
big5 = fromOldCode 846900323733667
big5I :: LabeledGraphI ConciseGraph Node
big5I = conciseGraphI 5

force6d :: ConciseGraph
force6d = fromOldCode 3936965
force6dI :: LabeledGraphI ConciseGraph Node
force6dI = conciseGraphI 4

force7d :: ConciseGraph
force7d = fromOldCode 3937932
force7dI :: LabeledGraphI ConciseGraph Node
force7dI = conciseGraphI 4

force8d :: ConciseGraph
force8d = fromOldCode 4019848
force8dI :: LabeledGraphI ConciseGraph Node
force8dI = conciseGraphI 4

force9d :: ConciseGraph
force9d = fromOldCode 4019736
force9dI :: LabeledGraphI ConciseGraph Node
force9dI = conciseGraphI 4

alloc1 :: ConciseGraph
alloc1 = fromOldCode 15589
alloc1I :: LabeledGraphI ConciseGraph Node
alloc1I = conciseGraphI 3

alloc2 :: ConciseGraph
alloc2 = fromOldCode 3937822
alloc2I :: LabeledGraphI ConciseGraph Node
alloc2I = conciseGraphI 4

alloc3 :: ConciseGraph
alloc3 = fromOldCode 3955401
alloc3I :: LabeledGraphI ConciseGraph Node
alloc3I = conciseGraphI 4

alloc4 :: ConciseGraph
alloc4 = fromOldCode 4019736
alloc4I :: LabeledGraphI ConciseGraph Node
alloc4I = conciseGraphI 4

{- crazy and crazier are simple patterns that can be solved by winding up the
01 spiral. There are elements that see the complete spikes of the spiral. -}
craz Zero = [(0,0),(0,5),(2,3),(2,4),(5,2),(5,6),(6,1)]
craz One = [(1,1),(1,3),(1,4),(1,5),(3,2),(4,0),(4,6)]
crazy :: LMapGraph Int
crazy = mapFromFunction craz
crazyI :: LabeledGraphI (LMapGraph Int) Int
crazyI = lMapGraphI

crazer Zero = [(0,0),(0,7),(2,3),(2,4),(5,2),(5,6),(6,1),(7,5)]
crazer One = [(1,1),(1,3),(1,4),(1,5),(3,2),(3,7),(4,0),(4,6)]
crazier :: LMapGraph Int
crazier = mapFromFunction craz
crazierI :: LabeledGraphI (LMapGraph Int) Int
crazierI = lMapGraphI

{- issues can not be solved by winding up the 01 spiral. There is no node that sees the complete spikes -}
iss Zero = [(0,0),(0,2),(2,1),(2,3),(2,4)]
iss One = [(1,0),(1,1),(1,4),(3,2),(4,3)]
issues :: LMapGraph Int
issues = mapFromFunction iss
issuesI :: LabeledGraphI (LMapGraph Int) Int
issuesI = lMapGraphI

{- interesting patter generated by unfolding -}
unf Zero = [(0,0),(0,2),(0,5),(2,1),(3,4),(5,3)]
unf One = [(1,1),(1,2),(1,3),(1,4),(4,0),(4,5)]
unfolded :: LMapGraph Int
unfolded = mapFromFunction unf
unfoldedI :: LabeledGraphI (LMapGraph Int) Int
unfoldedI = lMapGraphI
