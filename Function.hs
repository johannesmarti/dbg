module Function (
  Function,
  allFunctions,
) where

import Data.Map.Strict as Map
import Data.Set as Set

type Function x y = Map x y

allFunctions :: (Ord x, Ord y) => Set x -> Set y -> [Function x y]
allFunctions domain codomain = allFunctions' (Set.toList domain) (Set.toList codomain)

allFunctions' :: (Ord x, Ord y) => [x] -> [y] -> [Function x y]
allFunctions' domain codomain =
  Prelude.map (Prelude.foldl Map.union Map.empty) (mapM (mappingsTo codomain) domain) where
    mappingsTo list x = Prelude.map (Map.singleton x) list

