module Examples.Patterns (
  triple,
  hamburger, hamburgerInterface,
  force2d,
  force3d, force3dInterface,
  allPaths,
  testPattern,
  strange3,
  strictDet,
  cayleySchreck,
  cayleySchreckSize,
  slowFourConcise, slowFourConciseInterface, slowFourConciseSize,
  celtic,
  halfCeltic,
  slowSquare, slowSquareInterface,
  difficult, difficultInterface,
  deadEnd, deadEndInterface,
  deadEndWithoutEnd, deadEndWithoutEndInterface,
  slowLifting, slowLiftingInterface,
  goesWrong, goesWrongInterface,
  complicatedPos,
  complicatedNeg,
  unsound, unsoundInterface,
  noPath, noPathInterface,
  notQuitePath, notQuitePathInterface,
  zoComp, zoCompInterface,
  ex1, ex1Interface,
  force4d, force4dInterface,
  force4d',
  force5d, force5dInterface,
  force5d',
  ex2, ex2Interface,
  ex3, ex3Interface,
  ex4, ex4Interface,
  ex5, ex5Interface,
  uh, uhInterface,
  study, studyInterface,
  growingLifting, growingLiftingInterface,
  biggest, biggestInterface,
  alsoBig, alsoBigInterface,
  big5, big5Interface,
  force6d, force6dInterface,
  force7d, force7dInterface,
  force8d, force8dInterface,
  force9d, force9dInterface,
  alloc1, alloc1Interface,
  alloc2, alloc2Interface,
  alloc3, alloc3Interface,
  crazy, crazyInterface,
  crazier, crazierInterface,
  issues, issuesInterface,
  strange, strangeInterface,
  unfolded, unfoldedInterface,
  unfolded2, unfolded2Interface,
  b1ef5, b1ef5Interface,
  specialUnfold, specialUnfoldInterface,
  weaklyDeterministic, weaklyDeterministicInterface,
) where

import qualified Data.Set as Set

import CommonLabeledGraphTypes
import LabeledGraphInterface
import PrettyNode
import BitGraph
import ConciseGraph
import qualified PairGraph

trap Zero = [('a','a'),('a','c'),('c','b'),('c','a')]
trap One = [('b','b'),('b','c'),('c','a'),('a','c')]
triple :: LabeledMapGraph Char
triple = mapFromFunction trap

hb Zero = [('a','a'),('a','c'),('c','b'),('b','c'),('b','a'),('c','a')]
hb One = [('b','b'),('b','c'),('c','a'),('a','c'),('a','b'),('c','b')]
hamburger :: LabeledMapGraph Char
hamburger = mapFromFunction hb
hamburgerInterface :: LabeledGraphInterface (LabeledMapGraph Char) Char
hamburgerInterface = labeledMapGraphInterface

data N = Co | OZ | OO
  deriving (Eq,Ord,Show)
instance PrettyNode N where
  pretty n = show n

f2d Zero = [(Co,Co),(Co,OZ),(Co,OO)]
f2d One = [(OO,OO),(OO,OZ),(OZ,Co)]
force2d :: LabeledMapGraph N
force2d = mapFromFunction f2d

f3d Zero = [('a','a'),('a','c'),('c','a'),('c','b')]
f3d One = [('a','c'),('b','a'),('b','b')]
force3d :: LabeledMapGraph Char
force3d = mapFromFunction f3d
force3dInterface :: LabeledGraphInterface (LabeledMapGraph Char) Char
force3dInterface = labeledMapGraphInterface

noPat1 Zero = [('a','a'),('a','c'),('c','b'),('b','a'),('b','c')]
noPat1 One = [('a','c'),('a','b'),('c','a'),('b','a'),('b','b')]
noPattern1 :: LabeledMapGraph Char
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
allPaths :: LabeledMapGraph Char
allPaths = mapFromFunction ap

test Zero = [('a','a'),('a','c'),('c','b'),('b','a'),('b','c')]
test One = [('a','c'),('a','b'),('c','a'),('b','a'),('b','b')]
testPattern :: LabeledMapGraph Char
testPattern = mapFromFunction test

s3 Zero = [('a','a'),('a','b'),('b','a'),('b','c'),('c','b')]
s3 One = [('b','b'),('b','c'),('c','c'),('c','a')]
strange3 :: LabeledMapGraph Char
strange3 = mapFromFunction s3

sd Zero = [('a','a'),('a','c'),('b','a'),('b','c'),('c','b')]
sd One = [('a','c'),('a','b'),('b','a'),('b','b'),('c','a'),('c','b')]
strictDet :: LabeledMapGraph Char
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
slowFourConciseInterface :: LabeledGraphInterface ConciseGraph Node
slowFourConciseInterface = conciseGraphInterface slowFourConciseSize

cel Zero = [('a','a'),('a','b'),
            ('b','b'),('b','c'),
            ('c','a'),('c','c')]
cel One  =  [('a','a'),('a','b'),
            ('b','b'),('b','c'),
            ('c','a'),('c','c')]
celtic :: LabeledMapGraph Char
celtic = mapFromFunction cel

hcel Zero = [('a','a'),('a','b'),
            ('b','b'),('b','c'),
            ('c','a'),('c','c')]
hcel One = [('a','b'),('b','c'),('c','a')]
halfCeltic :: LabeledMapGraph Char
halfCeltic = mapFromFunction hcel

-- 3569496551
slowSquare' Zero = [('a', 'a'), ('a', 'b'), ('a', 'c'), ('b', 'b'), ('b', 'c'), ('b', 'd'), ('c', 'a'), ('d', 'a'), ('d', 'b')]
slowSquare' One = [('a', 'b'), ('b', 'c'), ('b', 'd'), ('c', 'c'), ('d', 'a'), ('d', 'c'), ('d', 'd')]
slowSquare :: LabeledMapGraph Char
slowSquare = mapFromFunction slowSquare'
slowSquareInterface :: LabeledGraphInterface (LabeledMapGraph Char) Char
slowSquareInterface = labeledMapGraphInterface

deadEnd :: ConciseGraph
deadEnd = fromOldCode 4072604
deadEndInterface :: LabeledGraphInterface ConciseGraph Node
deadEndInterface = (conciseGraphInterface 4)

deadEndWithoutEnd :: LabeledMapGraph Node
deadEndWithoutEnd = labeledMapSubgraphFromLabeledGraph deadEndInterface deadEnd (Set.fromList [0,1,3])
deadEndWithoutEndInterface :: LabeledGraphInterface (LabeledMapGraph Node) Node
deadEndWithoutEndInterface = labeledMapGraphInterface

{- is not construction deterministic. -}
slowLifting :: ConciseGraph 
slowLifting = fromOldCode 4966674
slowLiftingInterface :: LabeledGraphInterface ConciseGraph Node
slowLiftingInterface = conciseGraphInterface 4

{- Here it seems hard to find a homo -}
difficult :: ConciseGraph
difficult = fromOldCode 2063974806
difficultInterface :: LabeledGraphInterface ConciseGraph Node
difficultInterface = conciseGraphInterface 4

{- does not have path condition but is not construction deterministic -}
goesWrong :: ConciseGraph
goesWrong = fromOldCode 1612382568
goesWrongInterface :: LabeledGraphInterface ConciseGraph Node
goesWrongInterface = conciseGraphInterface 4

-- complicated
complicatedPos' Zero = [('a', 'a'), ('a', 'c'), ('c', 'a'), ('c', 'b'), ('b', 'a'), ('b', 'c')]
complicatedPos' One = [('a', 'c'), ('a', 'b'), ('c', 'a'), ('c', 'b'), ('b', 'a'), ('b', 'b')]
complicatedPos :: LabeledMapGraph Char
complicatedPos = mapFromFunction complicatedPos'

-- complicated
complicatedNeg' Zero = [('a', 'a'), ('a', 'c'), ('c', 'a'), ('c', 'b'), ('b', 'a'), ('b', 'c')]
complicatedNeg' One = [('a', 'c'), ('a', 'b'), ('c', 'a'), ('c', 'b'), ('b', 'c'), ('b', 'b')]
complicatedNeg :: LabeledMapGraph Char
complicatedNeg = mapFromFunction complicatedNeg'

{- This shows that the unsound filter for the lifting is indeed unsound. -}
unsound :: ConciseGraph
unsound = fromOldCode 2063931814
unsoundInterface :: LabeledGraphInterface ConciseGraph Node
unsoundInterface = conciseGraphInterface 4

{- The following pattern is not construction deterministic but also does not satisfy the path condition -}
noPath :: ConciseGraph
noPath = fromOldCode 988302
noPathInterface :: LabeledGraphInterface ConciseGraph Node
noPathInterface = conciseGraphInterface 4

{- This pattern satisfies the path condition up to words of length 2, but not for longer words. It is construction deterministic. -}
notQuitePath :: ConciseGraph
notQuitePath = fromOldCode 57450828
notQuitePathInterface :: LabeledGraphInterface ConciseGraph Node
notQuitePathInterface = conciseGraphInterface 4

{- In this pattern there is a 0-loop that reaches all other nodes over a 0-path and similarly for 1. But it does not satify the path condition. It is not construction deterministic. -}
zoComp :: ConciseGraph
zoComp = fromOldCode 23617753
zoCompInterface :: LabeledGraphInterface ConciseGraph Node
zoCompInterface = conciseGraphInterface 4

{- just a random example -}
e1 Zero = [('a','a'),('a','x'),('x','u'),('u','v'),('v','x'),('a','y'),('y','b')]
e1 One = [('b','b'),('b','a'),('b','x'),('b','u'),('b','v'),('x','y')]
ex1 :: LabeledMapGraph Char
ex1 = mapFromFunction e1
ex1Interface :: LabeledGraphInterface (LabeledMapGraph Char) Char
ex1Interface = labeledMapGraphInterface

{- Just some pattern with a homo at 4. -}
force4d :: ConciseGraph
force4d = fromOldCode 201822290
force4dInterface :: LabeledGraphInterface ConciseGraph Node
force4dInterface = conciseGraphInterface 4

force4d' :: ConciseGraph
force4d' = ConciseGraph.fromLabeledBitGraph 4 (PairGraph.fromFunction fct) where
  fct Zero = PairGraph.graphOfLabel pairGraph Zero
  fct One  = BitGraph.delArc 4 (PairGraph.graphOfLabel pairGraph One) (2,2)
  pairGraph = ConciseGraph.toLabeledBitGraph 4 force4d

{- Just some pattern with a homo at 5. -}
force5d :: ConciseGraph
force5d = fromOldCode 201822534
force5dInterface :: LabeledGraphInterface ConciseGraph Node
force5dInterface = conciseGraphInterface 4

force5d' :: ConciseGraph
force5d' = ConciseGraph.fromLabeledBitGraph 4 (PairGraph.fromFunction fct) where
  fct Zero = PairGraph.graphOfLabel pairGraph Zero
  fct One  = BitGraph.delArc 4 (BitGraph.delArc 4 (PairGraph.graphOfLabel pairGraph One) (2,2)) (2,2)
  pairGraph = ConciseGraph.toLabeledBitGraph 4 force5d

{- another random example -}
e2 Zero = [('a','a'),('a','o'),('a','x'),('a','z'),('o','u'),('x','v'),('x','i'),('z','b'),('u','y'),('u','v')]
e2 One = [('b','b'),('b','i'),('b','y'),('b','a'),('i','v'),('y','z'),('y','o'),('y','u'),('y','x'),('v','u')]
ex2 :: LabeledMapGraph Char
ex2 = mapFromFunction e2
ex2Interface :: LabeledGraphInterface (LabeledMapGraph Char) Char
ex2Interface = labeledMapGraphInterface

{- An example in which initially all nodes are 1 00-step away from a but then in the first lifting [y b] is only reachable via a 000-path. -}
e3 Zero = [('a','a'),('a','x'),('a','y'),('x','u'),('x','b'),('u','y'),('u','b')]
e3 One = [('b','b'),('b','y'),('b','u'),('b','a'),('y','x')]
ex3 :: LabeledMapGraph Char
ex3 = mapFromFunction e3
ex3Interface :: LabeledGraphInterface (LabeledMapGraph Char) Char
ex3Interface = labeledMapGraphInterface

{- One more example to play around with -}
e4 Zero = [('a','a'),('a','x'),('a','u'),('x','b'),('x','y'),('u','v')]
e4 One = [('b','b'),('b','y'),('b','v'),('y','a'),('v','x'),('v','u')]
ex4 :: LabeledMapGraph Char
ex4 = mapFromFunction e4
ex4Interface :: LabeledGraphInterface (LabeledMapGraph Char) Char
ex4Interface = labeledMapGraphInterface

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
ex5 :: LabeledMapGraph String
ex5 = mapFromFunction e5
ex5Interface :: LabeledGraphInterface (LabeledMapGraph String) String
ex5Interface = labeledMapGraphInterface

{- It satisfies the path condition and is not construction deterministic. -}
uhf Zero =
  [("a","a"),("a","c'"),("a","a'"),("a","c''"),("a","c'''"),
   ("c'","b"),
   ("a'","c'''")]
uhf One =
  [("b","b"),("b","c''"),("b","c'"),("b","c'''"),
   ("c''","a"),("c''","a'"),
   ("c'''","a'")]
uh :: LabeledMapGraph String
uh = mapFromFunction uhf
uhInterface :: LabeledGraphInterface (LabeledMapGraph String) String
uhInterface = labeledMapGraphInterface

stud Zero = [('b','a'),('c','a'),('c','b'),('c','c')]
stud One = [('a','a'),('a','b'),('b','c')]
study :: LabeledMapGraph Char
study = mapFromFunction stud
studyInterface :: LabeledGraphInterface (LabeledMapGraph Char) Char
studyInterface = labeledMapGraphInterface

sld = ['a', 'b', 'c']
sl Zero = [('a','a'),('a','b'),('b','c'),('c','b'),('c','c')]
sl One = [(x,y) | x <- sld, y <- sld] 
growingLifting :: LabeledMapGraph Char
growingLifting = mapFromFunction sl
growingLiftingInterface :: LabeledGraphInterface (LabeledMapGraph Char) Char
growingLiftingInterface = labeledMapGraphInterface

{- This pattern is the pattern such that it has a homo for a maximal dimension among all the patterns of size 4 -}
biggest :: ConciseGraph
biggest = fromOldCode 2458141589
biggestInterface :: LabeledGraphInterface ConciseGraph Node
biggestInterface = conciseGraphInterface 4

alsoBig :: ConciseGraph
alsoBig = fromOldCode 675781230
alsoBigInterface :: LabeledGraphInterface ConciseGraph Node
alsoBigInterface = conciseGraphInterface 4

big5 :: ConciseGraph
big5 = fromOldCode 846900323733667
big5Interface :: LabeledGraphInterface ConciseGraph Node
big5Interface = conciseGraphInterface 5

force6d :: ConciseGraph
force6d = fromOldCode 3936965
force6dInterface :: LabeledGraphInterface ConciseGraph Node
force6dInterface = conciseGraphInterface 4

force7d :: ConciseGraph
force7d = fromOldCode 3937932
force7dInterface :: LabeledGraphInterface ConciseGraph Node
force7dInterface = conciseGraphInterface 4

force8d :: ConciseGraph
force8d = fromOldCode 4019848
force8dInterface :: LabeledGraphInterface ConciseGraph Node
force8dInterface = conciseGraphInterface 4

force9d :: ConciseGraph
force9d = fromOldCode 4019736
force9dInterface :: LabeledGraphInterface ConciseGraph Node
force9dInterface = conciseGraphInterface 4

alloc1 :: ConciseGraph
alloc1 = fromOldCode 15589
alloc1Interface :: LabeledGraphInterface ConciseGraph Node
alloc1Interface = conciseGraphInterface 3

alloc2 :: ConciseGraph
alloc2 = fromOldCode 3937822
alloc2Interface :: LabeledGraphInterface ConciseGraph Node
alloc2Interface = conciseGraphInterface 4

alloc3 :: ConciseGraph
alloc3 = fromOldCode 3955401
alloc3Interface :: LabeledGraphInterface ConciseGraph Node
alloc3Interface = conciseGraphInterface 4

alloc4 :: ConciseGraph
alloc4 = fromOldCode 4019736
alloc4Interface :: LabeledGraphInterface ConciseGraph Node
alloc4Interface = conciseGraphInterface 4

{- crazy and crazier are simple patterns that can be solved by winding up the
01 spiral. There are elements that see the complete spikes of the spiral. -}
craz Zero = [(0,0),(0,5),(2,3),(2,4),(5,2),(5,6),(6,1)]
craz One = [(1,1),(1,3),(1,4),(1,5),(3,2),(4,0),(4,6)]
crazy :: LabeledMapGraph Int
crazy = mapFromFunction craz
crazyInterface :: LabeledGraphInterface (LabeledMapGraph Int) Int
crazyInterface = labeledMapGraphInterface

crazer Zero = [(0,0),(0,7),(2,3),(2,4),(5,2),(5,6),(6,1),(7,5)]
crazer One = [(1,1),(1,3),(1,4),(1,5),(3,2),(3,7),(4,0),(4,6)]
crazier :: LabeledMapGraph Int
crazier = mapFromFunction craz
crazierInterface :: LabeledGraphInterface (LabeledMapGraph Int) Int
crazierInterface = labeledMapGraphInterface

{- issues can not be solved by winding up the 01 spiral. There is no node that sees the complete spikes -}
iss Zero = [(0,0),(0,2),(2,1),(2,3),(2,4)]
iss One = [(1,0),(1,1),(1,4),(3,2),(4,3)]
issues :: LabeledMapGraph Int
issues = mapFromFunction iss
issuesInterface :: LabeledGraphInterface (LabeledMapGraph Int) Int
issuesInterface = labeledMapGraphInterface

str Zero = [(0,0),(0,2),(2,3),(3,1),(3,4)]
str One = [(1,1),(1,4),(1,3),(4,0),(4,2)]
strange :: LabeledMapGraph Int
strange = mapFromFunction str
strangeInterface :: LabeledGraphInterface (LabeledMapGraph Int) Int
strangeInterface = labeledMapGraphInterface

{- interesting patter generated by unfolding -}
unf Zero = [(0,0),(0,2),(0,5),(2,1),(3,4),(5,3)]
unf One = [(1,1),(1,2),(1,3),(1,4),(4,0),(4,5)]
unfolded :: LabeledMapGraph Int
unfolded = mapFromFunction unf
unfoldedInterface :: LabeledGraphInterface (LabeledMapGraph Int) Int
unfoldedInterface = labeledMapGraphInterface

{- interesting patter generated by unfolding -}
unf2 Zero = [(0,0),(0,2),(2,1),(2,3)]
unf2 One = [(1,1),(1,2),(1,3),(3,0)]
unfolded2 :: LabeledMapGraph Int
unfolded2 = mapFromFunction unf2
unfolded2Interface :: LabeledGraphInterface (LabeledMapGraph Int) Int
unfolded2Interface = labeledMapGraphInterface

b1ef5Fct Zero = [('a','a'),('a','e'),('a','c'),('e','f'),('c','b'),('c','d')]
b1ef5Fct One = [('b','b'),('b','d'),('d','f'),('b','e'),('d','c'),('f','a')]
b1ef5 :: LabeledMapGraph Char
b1ef5 = mapFromFunction b1ef5Fct
b1ef5Interface :: LabeledGraphInterface (LabeledMapGraph Char) Char
b1ef5Interface = labeledMapGraphInterface

su Zero = [('a','a'),('a','c'),('a','e'),('c','d'),('c','f'),('e','b'),('e','g')]
su One = [('b','b'),('b','a'),('b','f'),('b','g'),('d','c'),('f','e'),('g','d')]
specialUnfold :: LabeledMapGraph Char
specialUnfold = mapFromFunction su
specialUnfoldInterface :: LabeledGraphInterface (LabeledMapGraph Char) Char
specialUnfoldInterface = labeledMapGraphInterface

{- This pattern is not construction deterministic for any partition, but it is
construction deterministic for the anti-chain {1}, {0,1}, {0,3}. -}
weaklyDeterministic :: ConciseGraph
weaklyDeterministic = fromCode 4 287515978
weaklyDeterministicInterface :: LabeledGraphInterface ConciseGraph Node
weaklyDeterministicInterface = conciseGraphInterface 4
