module ConciseGraph (
  ConciseGraph,
  conciseGraphI,
  Size,
  Node,
  nodes,
  fromLBitGraph,
  toLBitGraph,
  hasBitForArc,
  isNode,
  allGraphsOfSize,
  totalGraph,
  relationOfLabel,
  hasBothFp,
  noDoubleRefl,
  notTrivial,
  caleyGraphOfConcise,
  pathConditionConcise,
  showem,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

import BitGraph (BitGraph,Node,Size,nodes)
import PairGraph
import CaleyGraph
import CommonLGraphTypes
import LabeledGraph
import Pretty

type ConciseGraph = Integer

conciseGraphI :: Size -> LabeledGraphI ConciseGraph Node
conciseGraphI size = LabeledGraph.interfaceFromSuccPredPretty
                       (dom size) (succs size) (preds size)
                       (\_ n -> pretty n)

dom :: Size -> ConciseGraph -> Set.Set Node
dom size bitset = assert (isValidBitset size bitset) $ Set.fromList (nodes size)

succs :: Size -> ConciseGraph -> MapFunction Node
succs size bitset label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasBitForArc size bitset (node,label,v)) (nodes size)

preds :: Size -> ConciseGraph -> MapFunction Node
preds size bitset label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasBitForArc size bitset (v,label,node)) (nodes size)

fromLBitGraph :: Size -> LBitGraph -> ConciseGraph
fromLBitGraph s bg =
  let
    zeroWord = zeroGraph bg
    oneWord = oneGraph bg
    cg = zeroWord .|. shiftL oneWord (s * s)
  in cg

toLBitGraph :: Size -> ConciseGraph -> LBitGraph
toLBitGraph size cg = PairGraph.fromFunction (relationOfLabel size cg)

nullConciseGraph :: ConciseGraph
nullConciseGraph = zeroBits

numBits :: Size -> Int
numBits size = 2 * size * size

totalGraph :: Size -> ConciseGraph
totalGraph size =
  (shiftL 1 (numBits size)) - 1

allGraphsOfSize :: Size -> [ConciseGraph]
allGraphsOfSize n = [nullConciseGraph .. totalGraph n]

isValidBitset :: Size -> ConciseGraph -> Bool
isValidBitset size bitset = bitset <= totalGraph size

isNode :: Size -> Node -> Bool
isNode size node = 0 <= node && node <= size

-- Maybe we should get rid of all these asserts here!
hasBitForArc :: Size -> ConciseGraph -> Arc Node -> Bool
hasBitForArc size bitset (from,label,to) = assert (isValidBitset size bitset) $
                                           assert (isNode size from) $
                                           assert (isNode size to) $ let
  offset = if label == Zero then 0 else size * size
  position = from * size + to
  index = offset + position
    in testBit bitset index

relationOfLabel :: Size -> ConciseGraph -> Label -> BitGraph
relationOfLabel size bitset label = assert (isValidBitset size bitset) $ let
  offset = size * size
  bitmask = (shiftL 1 offset) - 1
    in if label == Zero
         then bitset .&. bitmask
         else shiftR bitset offset

diagonal :: Size -> Label -> ConciseGraph
diagonal size label = shiftL baseDiagonal offset where
  offset = if label == Zero then 0 else size * size
  shifter pattern = shiftL pattern (size + 1) .|. 1
  nTimes n op start = if n == 0 then start else nTimes (n - 1) op (op start)
  baseDiagonal = nTimes (size - 1) shifter 1

hasNoFp :: Size -> Label -> ConciseGraph -> Bool
hasNoFp size label word = word .&. diagonal size label == 0

hasBothFp :: Size -> ConciseGraph -> Bool
hasBothFp size word = not (hasNoFp size Zero word) && not (hasNoFp size One word)

doubleRefl :: Size -> Node -> ConciseGraph
doubleRefl size node = assert (isNode size node) $
                       pattern where
  offset = size * size
  position = node * size + node
  pattern = setBit (setBit 0 position) (offset + position)

noDoubleRefl :: Size -> ConciseGraph -> Bool
noDoubleRefl size word = all notDoubleReflAt [0 .. size-1] where
  notDoubleReflAt node = let pat = doubleRefl size node
                          in pat /= pat .&. word

notTrivial :: Size -> ConciseGraph -> Bool
notTrivial size word = hasBothFp size word && noDoubleRefl size word

caleyGraphOfConcise :: Size -> ConciseGraph -> CaleyGraph
caleyGraphOfConcise size = (caleyGraphOfLBitGraph size) . (toLBitGraph size)

pathConditionConcise :: Size -> ConciseGraph -> Bool
pathConditionConcise size = (isGood size) . (caleyGraphOfConcise size)

showem :: Size -> ConciseGraph -> String
showem size graph = unlines $ prettyLabeledGraph (conciseGraphI size) graph
