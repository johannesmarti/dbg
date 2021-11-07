module UnlabeledBitGraph (
  UnlabeledBitGraph,
  Node,
  Size,
  nodes,
  succsAsList,
  predsAsList,
  allGraphsOfSize,
  nullWord,
  totalGraph,
  hasArc,
  diagonal,
  hasUniv,
  hasUnivInDom,
  hasNoRefl,
  hasNoReflInDom,
  compose,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

type UnlabeledBitGraph = Word
type Node = Int
type Size = Int

nodes :: Size -> [Node]
nodes size = [0 .. size-1]

nullWord :: UnlabeledBitGraph
nullWord = zeroBits

numBits :: Size -> Int
numBits size = size * size

totalGraph :: Size -> UnlabeledBitGraph
totalGraph size =
  assert (enoughBits size) $
  (shiftL 1 (numBits size)) - 1

allGraphsOfSize :: Size -> [UnlabeledBitGraph]
allGraphsOfSize n = [nullWord .. totalGraph n]

enoughBits :: Size -> Bool
enoughBits size = 0 <= size && numBits size <= finiteBitSize nullWord

isValidBitset :: Size -> UnlabeledBitGraph -> Bool
isValidBitset size bitset = bitset <= totalGraph size

isNode :: Size -> Node -> Bool
isNode size node = 0 <= node && node <= size

maskForSuccs :: Size -> UnlabeledBitGraph
maskForSuccs size = (shiftL 1 size) - 1

-- Maybe we should get rid of all these asserts here!
hasArc :: Size -> UnlabeledBitGraph -> (Node,Node) -> Bool
hasArc size bitset (from, to) = assert (enoughBits size) $
                                assert (isValidBitset size bitset) $
                                assert (isNode size from) $
                                assert (isNode size to) $ let
  position = from * size + to
  index = position
    in testBit bitset index

succsAsList :: Size -> UnlabeledBitGraph -> Node -> [Node]
succsAsList size bitset node = assert (isNode size node) $
  filter (\v -> hasArc size bitset (node,v)) (nodes size)

predsAsList :: Size -> UnlabeledBitGraph -> Node -> [Node]
predsAsList size bitset node = assert (isNode size node) $
  filter (\v -> hasArc size bitset (v,node)) (nodes size)

succsAsBits :: Size -> UnlabeledBitGraph -> Node -> UnlabeledBitGraph
succsAsBits size bitset node = assert (isNode size node) $
  (shiftR bitset (node * size)) .&. maskForSuccs size

compose :: Size -> UnlabeledBitGraph -> UnlabeledBitGraph -> UnlabeledBitGraph
compose size a b = assert (enoughBits size) $
                   assert (isValidBitset size a) $
                   assert (isValidBitset size b) $ let
  succsOf node = foldl (.|.) 0 (map (succsAsBits size b) (succsAsList size a node))
  succsOfInPos node = shiftL (succsOf node) (node * size)
    in foldl (.|.) 0 (map succsOfInPos (nodes size))

bitsOfInternal :: Size -> [Node] -> UnlabeledBitGraph
bitsOfInternal size dom = foldl (\accum n -> accum .|. (shiftL bitsOfSuccsInSubset (n * size))) 0 dom where
  bitsOfSuccsInSubset = listToBitmask dom

listToBitmask :: [Node] -> UnlabeledBitGraph
listToBitmask = foldl setBit 0

diagonal :: Size -> UnlabeledBitGraph
diagonal size = assert (enoughBits size) $
                nTimes (size - 1) shifter 1 where
  shifter pattern = shiftL pattern (size + 1) .|. 1
  nTimes n op start = if n == 0 then start else nTimes (n - 1) op (op start)

diagonalInDom :: Size -> [Node] -> UnlabeledBitGraph
diagonalInDom size dom = bitsOfInternal size dom .&. diagonal size

hasNoRefl :: Size -> UnlabeledBitGraph -> Bool
hasNoRefl size word = word .&. diagonal size == 0

hasNoReflInDom :: Size -> [Node] -> UnlabeledBitGraph -> Bool
hasNoReflInDom size dom word = word .&. diagonalInDom size dom == 0

hasUniv :: Size -> UnlabeledBitGraph -> Bool
hasUniv size word = any isUniv (nodes size) where
  isUniv node = let mask = shiftL (maskForSuccs size) (size * node)
                  in word .&. mask == mask

hasUnivInDom :: Size -> [Node] -> UnlabeledBitGraph -> Bool
hasUnivInDom size dom word = any isUniv dom where
  isUniv node = let mask = shiftL (listToBitmask dom) (size * node)
                  in word .&. mask == mask
