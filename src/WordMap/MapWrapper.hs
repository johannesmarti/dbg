module WordMap.MapWrapper (
  WordMap,
  empty,
  combine,
  inDomain,
  forceLookup,
  WordMap.MapWrapper.lookup,
  insert,
  delete,
) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Label

type WordMap x = Map.Map [Label] x

empty :: WordMap x
empty = Map.empty

combine :: x -> WordMap x -> WordMap x -> WordMap x
combine lbl left right = Map.insert [] lbl (Map.union left' right') where
  left'  = Map.mapKeysMonotonic (Zero:) left
  right' = Map.mapKeysMonotonic (One :) right

inDomain :: [Label] -> WordMap x -> Bool
inDomain = Map.member

forceLookup :: [Label] -> WordMap x -> x
forceLookup key m = fromMaybe err (WordMap.MapWrapper.lookup key m) where
  err = error $ "node " ++ show key ++ " not in WordMap"

lookup :: [Label] -> WordMap x -> Maybe x
lookup = Map.lookup

insert :: [Label] -> x -> WordMap x -> WordMap x
insert = Map.insert

delete :: [Label] -> WordMap x -> WordMap x
delete = Map.delete
