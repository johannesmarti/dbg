module BitGraph (
  BitGraph,
  Size,
  bitGraphI,
  nodes,
  allGraphsOfSize,
  totalGraph,
  relationOfLabel,
  hasBothFp,
  noDoubleRefl,
  notTrivial,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

import Graph

type BitGraph = Word
type Size = Int

bitGraphI :: Size -> GraphI BitGraph Int
bitGraphI size = GraphI (dom size) (succs size) (preds size)

nodes :: Size -> [Int]
nodes size = [0 .. size-1]

dom :: Size -> Word -> Set.Set Int
dom size bitset = assert (enoughBits size) $
                  assert (isValidBitset size bitset) $ Set.fromList (nodes size)

succs :: Size -> Word -> MapFunction Int
succs size bitset label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasArc size bitset (node,label,v)) (nodes size)

preds :: Size -> Word -> MapFunction Int
preds size bitset label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasArc size bitset (v,label,node)) (nodes size)

nullWord :: Word
nullWord = zeroBits

numBits :: Size -> Int
numBits size = 2 * size * size

totalGraph :: Size -> Word
totalGraph size =
  assert (enoughBits size) $
  (shiftL 1 (numBits size)) - 1

allGraphsOfSize :: Size -> [Word]
allGraphsOfSize n = [nullWord .. totalGraph n]

enoughBits :: Size -> Bool
enoughBits size = 0 <= size && numBits size <= finiteBitSize nullWord

isValidBitset :: Size -> Word -> Bool
isValidBitset size bitset = bitset <= totalGraph size

isNode :: Size -> Int -> Bool
isNode size node = 0 <= node && node <= size

-- Maybe we should get rid of all these asserts here!
hasArc :: Size -> Word -> Arc Int -> Bool
hasArc size bitset (from,label,to) = assert (enoughBits size) $
                                     assert (isValidBitset size bitset) $
                                     assert (isNode size from) $
                                     assert (isNode size to) $ let
  offset = if label == Zero then 0 else size * size
  position = from * size + to
  index = offset + position
    in testBit bitset index

relationOfLabel :: Size -> Word -> Label -> Word
relationOfLabel size bitset label = assert (enoughBits size) $
                                 assert (isValidBitset size bitset) $ let
  offset = size * size
  bitmask = (shiftL 1 offset) - 1
    in if label == Zero
         then bitset .&. bitmask
         else shiftR bitset offset

{-
subgraph :: Size -> Word -> Set Int -> (Word,Size)
subgraph size bitset newDomain = (newBitset, newSize) where
-}

diagonal :: Size -> Label -> Word
diagonal size label = assert (enoughBits size) $
                      shiftL baseDiagonal offset where
  offset = if label == Zero then 0 else size * size
  shifter pattern = shiftL pattern (size + 1) .|. 1
  nTimes n op start = if n == 0 then start else nTimes (n - 1) op (op start)
  baseDiagonal = nTimes (size - 1) shifter 1

hasNoFp :: Size -> Label -> Word -> Bool
hasNoFp size label word = word .&. diagonal size label == 0

hasBothFp :: Size -> Word -> Bool
hasBothFp size word = not (hasNoFp size Zero word) && not (hasNoFp size One word)

doubleRefl :: Size -> Int -> Word
doubleRefl size node = assert (enoughBits size && isNode size node) $
                       pattern where
  offset = size * size
  position = node * size + node
  pattern = setBit (setBit 0 position) (offset + position)

noDoubleRefl :: Size -> Word -> Bool
noDoubleRefl size word = all notDoubleReflAt [0 .. size-1] where
  notDoubleReflAt node = let pat = doubleRefl size node
                          in pat /= pat .&. word

notTrivial :: Size -> Word -> Bool
notTrivial size word = hasBothFp size word && noDoubleRefl size word
