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
allFunctions domain codomain = allFunctions4 (Set.toList domain) (Set.toList codomain)

allFunctions' :: (Ord x, Ord y) => [x] -> [y] -> [Function x y]
allFunctions' domain codomain =
  Prelude.map (Prelude.foldr Map.union Map.empty)
              (mapM (mappingsTo codomain) domain) where
    mappingsTo list x = Prelude.map (Map.singleton x) list

allFunctions'' :: (Ord x, Ord y) => [x] -> [y] -> [Function x y]
allFunctions'' [] codomain = [Map.empty]
allFunctions'' (x:xs) codomain = let
  rec = allFunctions'' xs codomain
  forX y = Prelude.map (Map.insert x y) rec
    in concatMap forX codomain

allFunctions''' :: (Ord x, Ord y) => [x] -> [y] -> [Function x y]
allFunctions''' [] codomain = [Map.empty]
allFunctions''' (x:xs) codomain = let
  forX y = Prelude.map (Map.insert x y) (allFunctions''' xs codomain)
    in concatMap forX codomain

allFunctions4 :: (Ord x, Ord y) => [x] -> [y] -> [Function x y]
allFunctions4 [] codomain = [Map.empty]
allFunctions4 (x:xs) codomain = let
  rec = allFunctions4 xs codomain
  forR r = Prelude.map (\y -> Map.insert x y r) codomain
    in concatMap forR rec

