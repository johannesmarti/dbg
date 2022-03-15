module BitGraph (
  BitGraph,
  Node,
  Size,
  bitGraphI,
  fromArcs,
  nodes,
  succsAsList,
  predsAsList,
  allGraphsOfSize,
  hasBitForArc,
  nullWord,
  totalGraph,
  setArc,
  diagonal,
  --BitGraph.isRefl,
  isUniv,
  isUnivInDom,
  hasUniv,
  hasUnivInDom,
  hasNoRefl,
  hasNoReflInDom,
  hasReflAndUnivInMultiple,
  hasReflAndUnivInMultipleDom,
  compose,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

import Graph

type BitGraph = Integer
type Node = Int
type Size = Int

bitGraphI :: Size -> Graph.GraphI (BitGraph) Node
bitGraphI size = interfaceFromHasArcPretty (dom size)
                                           (hasBitForArc size)
                                           (\_ n -> show n) 

nodes :: Size -> [Node]
nodes size = [0 .. size-1]

dom :: Size -> BitGraph -> Set.Set Node
dom size graph = Set.fromList $ nodes size

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

allGraphsOfSize :: Size -> [BitGraph]
allGraphsOfSize n = [nullWord .. totalGraph n]

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
isReflAndUnivInMultiple size graph node = BitGraph.isRefl size graph node &&
  any (\m -> isUniv size m node) (multiples size graph)

hasReflAndUnivInMultiple :: Size -> BitGraph -> Bool
hasReflAndUnivInMultiple size graph = 
  any (isReflAndUnivInMultiple size graph) (nodes size)

isReflAndUnivInMultipleDom :: Size -> [Node] -> BitGraph -> Node -> Bool
isReflAndUnivInMultipleDom size dom graph node = BitGraph.isRefl size graph node &&
  any (\m -> isUnivInDom size dom m node) (multiples size graph)

hasReflAndUnivInMultipleDom :: Size -> [Node] -> BitGraph -> Bool
hasReflAndUnivInMultipleDom size dom graph =
  any (isReflAndUnivInMultipleDom size dom graph) dom
