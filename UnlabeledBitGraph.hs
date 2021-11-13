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
  setArc,
  diagonal,
  isRefl,
  isUnivInDom,
  hasUniv,
  hasUnivInDom,
  hasNoRefl,
  hasNoReflInDom,
  hasReflAndUnivInMultiple,
  hasReflAndUnivInMultipleDom,
  compose,
  prettyUnlabeled,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

import Graph (stdPrintSuccessors)

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
  index = from * size + to
    in testBit bitset index

setArc :: Size -> UnlabeledBitGraph -> (Node,Node) -> UnlabeledBitGraph
setArc size bitset (from, to) = assert (enoughBits size) $
                                assert (isValidBitset size bitset) $
                                assert (isNode size from) $
                                assert (isNode size to) $ let
  index = from * size + to
    in setBit bitset index

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
listToBitmask list = foldl setBit 0 list

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

isRefl :: Size -> UnlabeledBitGraph -> Node -> Bool
isRefl size graph node = hasArc size graph (node,node)

isUniv :: Size -> UnlabeledBitGraph -> Node -> Bool
isUniv size graph node = graph .&. mask == mask where
  mask = shiftL (maskForSuccs size) (size * node)

hasUniv :: Size -> UnlabeledBitGraph -> Bool
hasUniv size word = any (isUniv size word) (nodes size)

isUnivInDom :: Size -> [Node] -> UnlabeledBitGraph -> Node -> Bool
isUnivInDom size dom graph node = assert (all (< size) dom) $
  graph .&. mask == mask where
    mask = shiftL (listToBitmask dom) (size * node)

hasUnivInDom :: Size -> [Node] -> UnlabeledBitGraph -> Bool
hasUnivInDom size dom graph = any (isUnivInDom size dom graph) dom

multiples :: Size -> UnlabeledBitGraph -> Set.Set UnlabeledBitGraph
multiples size rel = generateMultiples rel (Set.singleton rel) where
  generateMultiples r accum = let
      next = compose size r rel
    in if next `Set.member` accum
         then accum
         else generateMultiples next (Set.insert next accum)

isReflAndUnivInMultiple :: Size -> UnlabeledBitGraph -> Node -> Bool
isReflAndUnivInMultiple size graph node = isRefl size graph node &&
  any (\m -> isUniv size m node) (multiples size graph)

hasReflAndUnivInMultiple :: Size -> UnlabeledBitGraph -> Bool
hasReflAndUnivInMultiple size graph = 
  any (isReflAndUnivInMultiple size graph) (nodes size)

isReflAndUnivInMultipleDom :: Size -> [Node] -> UnlabeledBitGraph -> Node -> Bool
isReflAndUnivInMultipleDom size dom graph node = isRefl size graph node &&
  any (\m -> isUnivInDom size dom m node) (multiples size graph)

hasReflAndUnivInMultipleDom :: Size -> [Node] -> UnlabeledBitGraph -> Bool
hasReflAndUnivInMultipleDom size dom graph =
  any (isReflAndUnivInMultipleDom size dom graph) dom

prettyUnlabeled :: Size -> (Node -> String) -> UnlabeledBitGraph -> [String]
prettyUnlabeled s printNode g = basePrinter s printNode (stdPrintSuccessors printNode) g

basePrinter :: Size -> (Node -> String) -> ([Node] -> String) -> UnlabeledBitGraph -> [String]
basePrinter s printNode printSuccessors g = let
    lineForNode v = (printNode v) ++ " < " ++ printSuccessors (succsAsList s g v)
  in fmap lineForNode (nodes s)
