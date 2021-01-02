module DeBruijn (
  deBruijnGraph,
) where

import Control.Exception.Base
import Data.Bits
import Data.Set as Set

import Graph

type Dimension = Int
type Node = Word

zeroes :: Word
zeroes = zeroBits

deBruijnGraph :: Dimension -> Graph Node
deBruijnGraph dimension = assert (dimension >= 1) $
                          assert (dimension <= finiteBitSize zeroes) $
  fromFunctions dom succ pred where
    dom = fromList [zeroes.. (DeBruijn.mask dimension)]
    succ Zero n = assert (isNode dimension n) $
      if isZeroNode dimension n
        then fromList [succZero dimension n, succOne dimension n]
        else Set.empty
    succ One  n = assert (isNode dimension n) $
      if isOneNode dimension n
        then fromList [succZero dimension n, succOne dimension n]
        else Set.empty
    pred Zero n = assert (isNode dimension n) $
      singleton (shift n (-1))
    pred One  n = assert (isNode dimension n) $
      singleton (setBit (shift n (-1)) (dimension - 1))

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
