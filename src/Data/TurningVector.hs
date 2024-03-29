module Data.TurningVector (
  offset,vector,
  fromVectorWithIndex,
  fromList,
  toList,
  turnForward,
  turnBackward,
  at,
  zipWithList,
  zipWithListM,
  zipReverseWithListM,
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

reverse :: TurningVector x -> TurningVector x
reverse (TurningVector o v) = TurningVector (V.length v - o - 1) (V.reverse v)

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

-- TODO: This function should maybe be implemented properly!
zipReverseWithListM :: Monad m => (x -> a -> m x) -> TurningVector x -> [a]
                               -> m (TurningVector x)
zipReverseWithListM f input list = do
  reverseOutput <- zipWithListM f (Data.TurningVector.reverse input) list
  return (Data.TurningVector.reverse reverseOutput)

zipWithListM :: Monad m => (x -> a -> m x) -> TurningVector x -> [a]
                           -> m (TurningVector x)
zipWithListM f (TurningVector o vec) list = newVec >>= (return . TurningVector o) where
  (before, after) = V.splitAt o vec
  vecLength = V.length after
  (batch,tail) = splitAt vecLength list
  batchVector = V.fromList batch
  batchLength = V.length batchVector
  newVec = 
    if null tail
       then let (toProcess,rest) = V.splitAt batchLength after
            in do updated <- V.zipWithM f toProcess batchVector
                  return (before V.++ updated V.++ rest)
       else do updated <- V.zipWithM f after batchVector
               zipVectorWithListM f (before V.++ updated) tail

zipVectorWithListM :: Monad m => (x -> a -> m x) -> V.Vector x -> [a]
                                -> m (V.Vector x)
zipVectorWithListM f vec list = let
    n = V.length vec
    (batch,tail) = splitAt n list
    batchVector = V.fromList batch
    batchLength = V.length batchVector
  in if batchLength < n
       then let (toProcess, rest) = V.splitAt batchLength vec
            in do updated <- V.zipWithM f toProcess batchVector
                  return (updated V.++ rest)
       else do updated <- V.zipWithM f vec batchVector
               zipVectorWithListM f updated tail
