module TurningVector (
  fromVectorWithIndex,
  fromList,
  toList,
  turnForward,
  turnBackward,
  at,
  zipWithList,
) where

import Debug.Trace

import Control.Exception.Base (assert)
import qualified Data.Vector as V

data TurningVector x = TurningVector {
  offset :: Int,
  vector :: V.Vector x
}

fromVectorWithIndex :: Int -> V.Vector x -> TurningVector x
fromVectorWithIndex index v =
  assert (not (null v) && index >= 0 && index < length v) $
         TurningVector index v

fromList :: [x] -> TurningVector x
fromList l = fromVectorWithIndex 0 (V.fromList l)

toList :: TurningVector x -> [x]
toList (TurningVector o v) = V.toList after ++ V.toList before where
  (before, after) = V.splitAt o v

ix :: TurningVector x -> Int -> Int
ix (TurningVector o v) i = (o + i) `mod` V.length v

turnForward :: TurningVector x -> TurningVector x
turnForward tv = TurningVector (ix tv 1) (vector tv)

turnBackward :: TurningVector x -> TurningVector x
turnBackward tv = TurningVector (ix tv (-1)) (vector tv)

at :: TurningVector x -> Int -> x
at tv i = (vector tv) V.! (ix tv i)

zipWithList :: (x -> a -> x) -> TurningVector x -> [a] -> TurningVector x
zipWithList f (TurningVector o vec) list = TurningVector o newVec where
  (before, after) = V.splitAt o vec
  vecLength = V.length after
  (batch,tail) = splitAt vecLength list
  batchVector = V.fromList batch
  batchLength = V.length batchVector
  newVec = 
    if null tail
       then let (toProcess,rest) = V.splitAt batchLength after
            in before V.++ (V.zipWith f toProcess batchVector) V.++ rest
       else zipVectorWithList f (before V.++ (V.zipWith f after batchVector)) tail

zipVectorWithList :: (x -> a -> x) -> V.Vector x -> [a] -> V.Vector x
zipVectorWithList f vec list = let
    n = V.length vec
    (batch,tail) = splitAt n list
    batchVector = V.fromList batch
    batchLength = V.length batchVector
  in if batchLength < n
       then let (toProcess, rest) = V.splitAt batchLength vec
            in (V.zipWith f toProcess batchVector) V.++ rest
       else zipVectorWithList f (V.zipWith f vec batchVector) tail
