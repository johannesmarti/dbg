module WordMap.MapWrapper (
  WordMap,
  empty,
  combine,
  inDomain,
  WordMap.MapWrapper.lookup,
  insert,
  delete,
) where

import qualified Data.Map.Strict as Map

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

lookup :: [Label] -> WordMap x -> Maybe x
lookup = Map.lookup

insert :: [Label] -> x -> WordMap x -> WordMap x
insert = Map.insert

delete :: [Label] -> WordMap x -> WordMap x
delete = Map.delete
