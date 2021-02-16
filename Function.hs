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
allFunctions' domain codomain =
  Prelude.map (Prelude.foldr Map.union Map.empty)
              (mapM (mappingsTo codomain) domain) where
    mappingsTo list x = Prelude.map (Map.singleton x) list

