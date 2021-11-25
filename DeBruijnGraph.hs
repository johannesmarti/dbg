module DeBruijnGraph (
  DBG,
  dbg,
  dbgI,
  nodeToList,
) where

import Control.Exception.Base
import Data.Bits
import Data.Char (intToDigit)
import Data.Set as Set
import Numeric (showHex, showIntAtBase)

import LabeledGraph hiding (domain,successors,predecessors)

type Dimension = Int
type Node = Word

zeroes :: Word
zeroes = zeroBits

newtype DBG = DBG {dimension :: Dimension}

-- TODO: Quite some speedup might be possible by explicitely implemeniting arcs!
dbgI :: LabeledGraphI DBG Node
dbgI = interfaceFromSuccPredPretty domain successors predecessors nodePrinter

domain :: DBG -> Set Node
domain (DBG dim) = fromList [zeroes .. (DeBruijn.mask dim)]

successors :: DBG -> MapFunction Node
successors (DBG dim) Zero n = assert (isNode dim n) $
      if isZeroNode dim n
        then fromList [succZero dim n, succOne dim n]
        else Set.empty
successors (DBG dim) One n = assert (isNode dim n) $
      if isOneNode dim n
        then fromList [succZero dim n, succOne dim n]
        else Set.empty

predecessors :: DBG -> MapFunction Node
predecessors (DBG dim) Zero n = assert (isNode dim n) $
      singleton (shift n (-1))
predecessors (DBG dim) One  n = assert (isNode dim n) $
      singleton (setBit (shift n (-1)) (dim - 1))

dbg :: Dimension -> DBG
dbg dim = assert (dim >= 1) $
          assert (dim <= finiteBitSize zeroes) $ DBG dim

size :: Dimension -> Node
size dimension = shift 1 dimension

mask :: Dimension -> Node
mask dimension = (DeBruijn.size dimension) - 1

isNode :: Dimension -> Node -> Bool
isNode dimension node = node < DeBruijn.size dimension

isOneNode :: Dimension -> Node -> Bool
isOneNode dimension node = testBit node (dimension - 1)

isZeroNode :: Dimension -> Node -> Bool
isZeroNode dimension node = not (isOneNode dimension node)

succZero :: Dimension -> Node -> Node
succZero dimension node = (shift node 1) .&. (DeBruijn.mask dimension)

succOne :: Dimension -> Node -> Node
succOne dimension node = setBit (succZero dimension node) 0

nodeToList :: Dimension -> Node -> [Label]
nodeToList dim node = assert (isNode dim node) $ reverse $
  Prelude.map setLabel [0 .. dim - 1] where
    setLabel i = if testBit node i then One else Zero

instance Show DBG where
  show g = showLG dbgI g

nodePrinter :: DBG -> Node -> String
nodePrinter g n = let str = showIntAtBase 2 intToDigit n ""
                      leadingZeros = replicate ((dimension g) - length str) '0'
                  in leadingZeros ++ str
