module Programs.Play (
  bits,
) where

import Data.Bits

a :: Word
a = zeroBits

b :: Word
b = setBit a (-2)

bits :: IO ()
bits = putStrLn (show b)
