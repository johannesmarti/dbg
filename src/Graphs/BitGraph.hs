module Graphs.BitGraph (
  BitGraph,
  Node,
  Size,
  bitGraphInterface,
  fromArcs,
  nodes,
  nodesSet,
  succsAsList,
  predsAsList,
  allabeledGraphsOfSize,
  hasBitForArc,
  nullWord,
  totalGraph,
  setArc,
  delArc,
  diagonal,
  isUniv,
  isUnivInDom,
  hasUniv,
  hasUnivInDom,
  hasNoRefl,
  hasNoReflInDom,
  reflexivesUnivInMultiple,
  hasReflAndUnivInMultiple,
  hasReflAndUnivInMultipleDom,
  compose,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

import Graphs.GraphInterface

type BitGraph = Integer
type Node = Int
type Size = Int

{-
 It is unclear whether BitGraph should store its size or whether the size
should be provided by the sourrounding code (for instance as a parameter to
bitGraphInterface). Currently, it is provided by the sourrounding code. This
has the following advantages:

1) It might be more efficient. Though I have the the strong suspicion that this
is a premature optimization.

2) In many contexts (for instance the 'compose' function, the '(PairGraph
BitGraph)' type from 'Graphs.CommonLabeledGraphTypes', or the nodes of a
CayleyGraph) there is an assumptions that all involved instances of BitGraph
are of the same size. In such context this can be nicely enforced by having
just one size variable that is applied to all BitGraphs. In case each BitGraph
stores it's own size it becomes something that would need to be asserted. But
then again such asserts might be a good thing since it allows us to actually
check in the code that the assumption of equal size is satisfied.

The clear disadvantage of not storing the size as part of the BitGraph is that
it makes the code using BitGraphs much more complex, because the size parameter
needs to be passed around independently from the BitGraphs.
-}

bitGraphInterface :: Size -> GraphInterface BitGraph Node
bitGraphInterface size = interfaceFromHasArcPretty (dom size)
                                           (hasBitForArc size)
                                           (\_ n -> show n) 

nodes :: Size -> [Node]
nodes size = [0 .. size-1]

nodesSet :: Size -> Set.Set Node
nodesSet = Set.fromList . nodes

dom :: Size -> BitGraph -> Set.Set Node
dom size _ = nodesSet size

fromArcs :: Size -> [(Node,Node)] -> BitGraph
fromArcs size arcs = bm where
  bm = foldl (setArc size) nullWord arcs

nullWord :: BitGraph
--nullWord = zeroBits
nullWord = 0

numBits :: Size -> Int
numBits size = size * size

totalGraph :: Size -> BitGraph
totalGraph size =
  (shiftL 1 (numBits size)) - 1

allabeledGraphsOfSize :: Size -> [BitGraph]
allabeledGraphsOfSize n = [nullWord .. totalGraph n]

isValidBitset :: Size -> BitGraph -> Bool
isValidBitset size bitset = bitset <= totalGraph size

isNode :: Size -> Node -> Bool
isNode size node = 0 <= node && node <= size

maskForSuccs :: Size -> BitGraph
maskForSuccs size = (shiftL 1 size) - 1

hasBitForArc :: Size -> BitGraph -> (Node,Node) -> Bool
hasBitForArc size bitset (from, to) = assert (isValidBitset size bitset) $
                                      assert (isNode size from) $
                                      assert (isNode size to) $ let
  index = from * size + to
    in testBit bitset index

setArc :: Size -> BitGraph -> (Node,Node) -> BitGraph
setArc size bitset (from, to) = assert (isValidBitset size bitset) $
                                assert (isNode size from) $
                                assert (isNode size to) $ let
  index = from * size + to
    in setBit bitset index

delArc :: Size -> BitGraph -> (Node,Node) -> BitGraph
delArc size bitset (from, to) = assert (isValidBitset size bitset) $
                                assert (isNode size from) $
                                assert (isNode size to) $ let
  index = from * size + to
    in clearBit bitset index

succsAsList :: Size -> BitGraph -> Node -> [Node]
succsAsList size bitset node = assert (isNode size node) $
  filter (\v -> hasBitForArc size bitset (node,v)) (nodes size)

predsAsList :: Size -> BitGraph -> Node -> [Node]
predsAsList size bitset node = assert (isNode size node) $
  filter (\v -> hasBitForArc size bitset (v,node)) (nodes size)

succsAsBits :: Size -> BitGraph -> Node -> BitGraph
succsAsBits size bitset node = assert (isNode size node) $
  (shiftR bitset (node * size)) .&. maskForSuccs size

compose :: Size -> BitGraph -> BitGraph -> BitGraph
compose size a b = assert (isValidBitset size a) $
                   assert (isValidBitset size b) $ let
  succsOf node = foldl (.|.) 0 (map (succsAsBits size b) (succsAsList size a node))
  succsOfInPos node = shiftL (succsOf node) (node * size)
    in foldl (.|.) 0 (map succsOfInPos (nodes size))

bitsOfInternal :: Size -> [Node] -> BitGraph
bitsOfInternal size dom = foldl (\accum n -> accum .|. (shiftL bitsOfSuccsInSubset (n * size))) 0 dom where
  bitsOfSuccsInSubset = listToBitmask dom

listToBitmask :: [Node] -> BitGraph
listToBitmask list = foldl setBit 0 list

diagonal :: Size -> BitGraph
diagonal size = nTimes (size - 1) shifter 1 where
  shifter pattern = shiftL pattern (size + 1) .|. 1
  nTimes n op start = if n == 0 then start else nTimes (n - 1) op (op start)

diagonalInDom :: Size -> [Node] -> BitGraph
diagonalInDom size dom = bitsOfInternal size dom .&. diagonal size

hasNoRefl :: Size -> BitGraph -> Bool
hasNoRefl size word = word .&. diagonal size == 0

hasNoReflInDom :: Size -> [Node] -> BitGraph -> Bool
hasNoReflInDom size dom word = word .&. diagonalInDom size dom == 0

isRefl :: Size -> BitGraph -> Node -> Bool
isRefl size graph node = hasBitForArc size graph (node,node)

isUniv :: Size -> BitGraph -> Node -> Bool
isUniv size graph node = graph .&. mask == mask where
  mask = shiftL (maskForSuccs size) (size * node)

hasUniv :: Size -> BitGraph -> Bool
hasUniv size word = any (isUniv size word) (nodes size)

isUnivInDom :: Size -> [Node] -> BitGraph -> Node -> Bool
isUnivInDom size dom graph node = assert (all (< size) dom) $
  graph .&. mask == mask where
    mask = shiftL (listToBitmask dom) (size * node)

hasUnivInDom :: Size -> [Node] -> BitGraph -> Bool
hasUnivInDom size dom graph = any (isUnivInDom size dom graph) dom

multiples :: Size -> BitGraph -> Set.Set BitGraph
multiples size rel = generateMultiples rel (Set.singleton rel) where
  generateMultiples r accum = let
      next = compose size r rel
    in if next `Set.member` accum
         then accum
         else generateMultiples next (Set.insert next accum)

isReflAndUnivInMultiple :: Size -> BitGraph -> Node -> Bool
isReflAndUnivInMultiple size graph node =
  Graphs.BitGraph.isRefl size graph node &&
    any (\m -> isUniv size m node) (multiples size graph)

reflexivesUnivInMultiple :: Size -> BitGraph -> Set.Set Node
reflexivesUnivInMultiple size graph = let
    mults = multiples size graph
    isGood node = Graphs.BitGraph.isRefl size graph node
                    && any (\m -> isUniv size m node) mults
  in Set.filter isGood (dom size graph)

hasReflAndUnivInMultiple :: Size -> BitGraph -> Bool
hasReflAndUnivInMultiple size graph = 
  any (isReflAndUnivInMultiple size graph) (nodes size)

isReflAndUnivInMultipleDom :: Size -> [Node] -> BitGraph -> Node -> Bool
isReflAndUnivInMultipleDom size dom graph node = 
  Graphs.BitGraph.isRefl size graph node &&
    any (\m -> isUnivInDom size dom m node) (multiples size graph)

hasReflAndUnivInMultipleDom :: Size -> [Node] -> BitGraph -> Bool
hasReflAndUnivInMultipleDom size dom graph =
  any (isReflAndUnivInMultipleDom size dom graph) dom
