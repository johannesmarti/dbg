module Graphs.ConciseGraph (
  ConciseGraph,
  conciseGraphInterface,
  Size,
  Node,
  fromCode,
  fromCode',
  estimateSize,
  fromOldCode,
  toCode,
  nodes,
  fromLabeledBitGraph,
  toLabeledBitGraph,
  hasBitForArc,
  setBitForArc,
  isNode,
  allLabeledGraphsOfSize,
  totalabeledGraph,
  relationOfLabel,
  hasBothFp,
  noDoubleRefl,
  notTrivial,
  showem,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

import Graphs.BitGraph (BitGraph,Node,Size,nodes)
import Graphs.PairGraph
import Graphs.CommonLabeledGraphTypes
import Graphs.LabeledGraphInterface
import Graphs.PrettyNode
import Data.Label

type ConciseGraph = Integer

conciseGraphInterface :: Size -> LabeledGraphInterface ConciseGraph Node
conciseGraphInterface size = iFromSuccPredPretty
                       (dom size) (succs size) (preds size)
                       (\_ n -> pretty n)

dom :: Size -> ConciseGraph -> Set.Set Node
dom size bitset = assert (isValidBitset size bitset) $ Set.fromList (nodes size)

succs :: Size -> ConciseGraph -> MapFunction Node
succs size bitset label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasBitForArc size bitset label (node,v)) (nodes size)

preds :: Size -> ConciseGraph -> MapFunction Node
preds size bitset label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasBitForArc size bitset label (v,node)) (nodes size)

converse :: Size -> Integer -> Integer
converse size cg = let
    cgi = (converseI (conciseGraphInterface size))
    zArcs = arcsOfLabel cgi cg Zero
    oArcs = arcsOfLabel cgi cg One
    withZ = foldl (\g a -> setBitForArc size g Zero a) nullConciseGraph zArcs
    withBoth = foldl (\g a -> setBitForArc size g One a) withZ oArcs
  in withBoth

fromCode :: Size -> Integer -> ConciseGraph
fromCode size code = converse size code

estimateSize :: Integer -> Size
estimateSize code = hack 0 where
  hack estimate = if isValidBitset estimate code
                         then estimate
                         else hack (estimate + 1)

fromCode' :: Integer -> ConciseGraph
fromCode' code = fromCode size code where
  size = estimateSize code

fromOldCode :: Integer -> ConciseGraph
fromOldCode = id

toCode :: Size -> ConciseGraph -> Integer
toCode size cg = converse size cg

fromLabeledBitGraph :: Size -> LabeledBitGraph -> ConciseGraph
fromLabeledBitGraph s bg =
  let
    zeroWord = zeroGraph bg
    oneWord = oneGraph bg
    cg = zeroWord .|. shiftL oneWord (s * s)
  in cg

toLabeledBitGraph :: Size -> ConciseGraph -> LabeledBitGraph
toLabeledBitGraph size cg = Graphs.PairGraph.fromFunction (relationOfLabel size cg)

nullConciseGraph :: ConciseGraph
nullConciseGraph = zeroBits

numBits :: Size -> Int
numBits size = 2 * size * size

totalabeledGraph :: Size -> ConciseGraph
totalabeledGraph size =
  (shiftL 1 (numBits size)) - 1

allLabeledGraphsOfSize :: Size -> [ConciseGraph]
allLabeledGraphsOfSize n = [nullConciseGraph .. totalabeledGraph n]

isValidBitset :: Size -> ConciseGraph -> Bool
isValidBitset size bitset = bitset <= totalabeledGraph size

isNode :: Size -> Node -> Bool
isNode size node = 0 <= node && node <= size

-- Maybe we should get rid of all these asserts here!
hasBitForArc :: Size -> ConciseGraph -> Label -> (Node,Node) -> Bool
hasBitForArc size bitset label (from,to) = assert (isValidBitset size bitset) $
                                           assert (isNode size from) $
                                           assert (isNode size to) $ let
  offset = if label == Zero then 0 else size * size
  position = from * size + to
  index = offset + position
    in testBit bitset index

setBitForArc :: Size -> ConciseGraph -> Label -> (Node,Node) -> ConciseGraph
setBitForArc size bitset label (from,to) = assert (isValidBitset size bitset) $
                                           assert (isNode size from) $
                                           assert (isNode size to) $ let
  offset = if label == Zero then 0 else size * size
  position = from * size + to
  index = offset + position
    in setBit bitset index

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

showem :: Size -> ConciseGraph -> String
showem size graph = unlines $ prettyLabeledGraph (conciseGraphInterface size) graph
