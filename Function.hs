module Function (
  Function,
  domain,
  range,
  applyFct,
  allFunctions,
) where

import Data.Map.Strict as Map
import Data.Set as Set

type Function x y = Map x y

domain :: Function x y -> Set x
domain = Map.keysSet

range :: Ord y => Function x y -> Set y
range = Set.fromList . Map.elems

applyFct :: Ord x => Function x y -> x -> y
applyFct f v = f ! v

allFunctions :: (Ord x, Ord y) => Set x -> Set y -> [Function x y]
allFunctions domain codomain = allFunctions' (Set.toList domain) (Set.toList codomain)

allFunctions' :: (Ord x, Ord y) => [x] -> [y] -> [Function x y]
allFunctions' [] codomain = [Map.empty]
allFunctions' (x:xs) codomain = let
  rec = allFunctions' xs codomain
  forR r = Prelude.map (\y -> Map.insert x y r) codomain
    in concatMap forR rec

