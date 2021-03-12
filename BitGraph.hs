module BitGraph (
  bitGraph,
  allGraphsOfSize,
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
enoughBits size = numBits size <= finiteBitSize nullWord

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
