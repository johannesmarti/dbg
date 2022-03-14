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
  slowFourConcise, slowFourConciseI,
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
  force5d, force5dI,
  ex2, ex2I,
  ex3, ex3I,
  ex4, ex4I,
  ex5, ex5I,
  uh, uhI,
  study, studyI,
  growingLifting, growingLiftingI,
  biggest, biggestI
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
cayleySchreck = 3942849
cayleySchreckSize :: Size
cayleySchreckSize = 4

{- This pattern is construction deterministic -}
slowFourConcise :: ConciseGraph
slowFourConcise = 4003476
slowFourConciseI :: LabeledGraphI ConciseGraph Node
slowFourConciseI = conciseGraphI 4

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
deadEnd = 4072604
deadEndI :: LabeledGraphI ConciseGraph Node
deadEndI = (conciseGraphI 4)

deadEndWithoutEnd :: LMapGraph Node
deadEndWithoutEnd = lMapSubgraphFromLGraph deadEndI deadEnd (Set.fromList [0,1,3])
deadEndWithoutEndI :: LabeledGraphI (LMapGraph Node) Node
deadEndWithoutEndI = lMapGraphI

{- is not construction deterministic. -}
slowLifting :: ConciseGraph 
slowLifting = 4966674
slowLiftingI :: LabeledGraphI ConciseGraph Node
slowLiftingI = conciseGraphI 4

{- Here it seems where hard to find a homo -}
difficult :: ConciseGraph
difficult = 2063974806
difficultI :: LabeledGraphI ConciseGraph Node
difficultI = conciseGraphI 4

{- does not have path condition but is not construction deterministic -}
goesWrong :: ConciseGraph
goesWrong = 1612382568
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
unsound = 2063931814
unsoundI :: LabeledGraphI ConciseGraph Node
unsoundI = conciseGraphI 4

{- The following pattern is not construction deterministic but also does not satisfy the path condition -}
noPath :: ConciseGraph
noPath = 988302
noPathI :: LabeledGraphI ConciseGraph Node
noPathI = conciseGraphI 4

{- This pattern satisfies the path condition up to words of length 2, but not for longer words. It is construction deterministic. -}
notQuitePath :: ConciseGraph
notQuitePath = 57450828
notQuitePathI :: LabeledGraphI ConciseGraph Node
notQuitePathI = conciseGraphI 4

{- In this pattern there is a 0-loop that reaches all other nodes over a 0-path and similarly for 1. But it does not satify the path condition. It is not construction deterministic. -}
zoComp :: ConciseGraph
zoComp = 23617753
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
force4d = 201822290
force4dI :: LabeledGraphI ConciseGraph Node
force4dI = conciseGraphI 4

{- Just some pattern with a homo at 5. -}
force5d :: ConciseGraph
force5d = 201822534
force5dI :: LabeledGraphI ConciseGraph Node
force5dI = conciseGraphI 4

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

{- It satisfies the path condition and is construction deterministic. -}
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

{- It satisfies the path condition and is construction deterministic. -}
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

{- Here it seems where hard to find a homo -}
biggest :: ConciseGraph
biggest = 2458141589
biggestI :: LabeledGraphI ConciseGraph Node
biggestI = conciseGraphI 4
