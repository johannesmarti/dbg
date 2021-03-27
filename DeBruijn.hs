module DeBruijn (
  DBG,
  dbg,
  dbgI,
) where

import Control.Exception.Base
import Data.Bits
import Data.Char (intToDigit)
import Data.Set as Set
import Numeric (showHex, showIntAtBase)

import Graph hiding (domain,successors,predecessors)

type Dimension = Int
type Node = Word

zeroes :: Word
zeroes = zeroBits

newtype DBG = DBG {dimension :: Dimension}

dbgI :: GraphI DBG Node
dbgI = GraphI domain successors predecessors

domain :: DBG -> Set Node
domain (DBG dim) = fromList [zeroes .. (DeBruijn.mask dim)]

successors :: DBG -> Graph.MapFunction Node
successors (DBG dim) Zero n = assert (isNode dim n) $
      if isZeroNode dim n
        then fromList [succZero dim n, succOne dim n]
        else Set.empty
successors (DBG dim) One n = assert (isNode dim n) $
      if isOneNode dim n
        then fromList [succZero dim n, succOne dim n]
        else Set.empty

predecessors :: DBG -> Graph.MapFunction Node
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

instance Show DBG where
  show g = unlines $ Graph.prettyGraph dbgI (nodePrinter g) g

nodePrinter :: DBG -> Node -> String
nodePrinter g n = let str = showIntAtBase 2 intToDigit n ""
                      leadingZeros = replicate ((dimension g) - length str) '0'
                  in leadingZeros ++ str
