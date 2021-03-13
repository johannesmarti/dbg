module BitGraph (
  bitGraph,
  allGraphsOfSize,
  hasBothFp,
  noDoubleRefl,
  notTrivial,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

import Graph

{- Might want to add to the graph a way of creating graphs from the arcAt
function. -}
bitGraph :: Int -> Word -> Graph Int
bitGraph size bitset = assert (enoughBits size) $
                       assert (isValidBitset size bitset) $
  fromFunctions dom succ pred where
    nodes = [0 .. size-1]
    isArc = hasArc size bitset
    dom = Set.fromList nodes
    succ label node = assert (isNode size node) $
                      Set.fromList $ filter (\v -> isArc (node,label,v)) nodes
    pred label node = assert (isNode size node) $
                      Set.fromList $ filter (\v -> isArc (v,label,node)) nodes
 
nullWord :: Word
nullWord = zeroBits

numBits :: Int -> Int
numBits size = 2 * size * size

totalGraph :: Int -> Word
totalGraph size =
  assert (enoughBits size) $
  (shiftL 1 (numBits size)) - 1

allGraphsOfSize :: Int -> [Word]
allGraphsOfSize n = [nullWord .. totalGraph n]

enoughBits :: Int -> Bool
enoughBits size = 0 <= size && numBits size <= finiteBitSize nullWord

isValidBitset :: Int -> Word -> Bool
isValidBitset size bitset = bitset <= totalGraph size

isNode :: Int -> Int -> Bool
isNode size node = 0 <= node && node <= size

-- Maybe we should get rid of all these asserts here!
hasArc :: Int -> Word -> Arc Int -> Bool
hasArc size bitset (from,label,to) = assert (enoughBits size) $
                                     assert (isValidBitset size bitset) $
                                     assert (isNode size from) $
                                     assert (isNode size to) $ let
  offset = if label == Zero then 0 else size * size
  position = from * size + to
  index = offset + position
    in testBit bitset index

diagonal :: Int -> Label -> Word
diagonal size label = assert (enoughBits size) $
                      shiftL baseDiagonal offset where
  offset = if label == Zero then 0 else size * size
  shifter pattern = shiftL pattern (size + 1) .|. 1
  nTimes n op start = if n == 0 then start else nTimes (n - 1) op (op start)
  baseDiagonal = nTimes (size - 1) shifter 1

hasNoFp :: Int -> Label -> Word -> Bool
hasNoFp size label word = word .&. diagonal size label == 0

hasBothFp :: Int -> Word -> Bool
hasBothFp size word = not (hasNoFp size Zero word) && not (hasNoFp size One word)

doubleRefl :: Int -> Int -> Word
doubleRefl size node = assert (enoughBits size && isNode size node) $
                       pattern where
  offset = size * size
  position = node * size + node
  pattern = setBit (setBit 0 position) (offset + position)

noDoubleRefl :: Int -> Word -> Bool
noDoubleRefl size word = all notDoubleReflAt [0 .. size-1] where
  notDoubleReflAt node = let pat = doubleRefl size node
                          in pat /= pat .&. word

notTrivial :: Int -> Word -> Bool
notTrivial size word = hasBothFp size word && noDoubleRefl size word
