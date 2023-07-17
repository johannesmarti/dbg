module Data.WordMaps.Coalgebraic (
  WordMap,
  empty,
  combine,
  inDomain,
  forceLookup,
  Data.WordMaps.Coalgebraic.lookup,
  insert,
  delete,
) where

import Data.Maybe (fromMaybe,isJust)

import Data.Label
import Data.WordTree

type WordMap x = WordTree (Maybe x)

empty :: WordMap x
empty = wordTreeFromGenerator gen where
  gen = WordTreeGenerator Nothing (const Nothing) (const Nothing)

combine :: x -> WordMap x -> WordMap x -> WordMap x
combine lbl left right = WordTreeNode (Just lbl) left right

inDomain :: [Label] -> WordMap x -> Bool
inDomain word wm = isJust $ Data.WordMaps.Coalgebraic.lookup word wm

forceLookup :: [Label] -> WordMap x -> x
forceLookup key m = fromMaybe err (Data.WordMaps.Coalgebraic.lookup key m) where
  err = error $ "node " ++ show key ++ " not in WordMap"

lookup :: [Label] -> WordMap x -> Maybe x
lookup key wt = labelOfWord wt key

insert :: [Label] -> x -> WordMap x -> WordMap x
insert key value wt = setWord (Just value) key wt

delete :: [Label] -> WordMap x -> WordMap x
delete key wt = setWord Nothing key wt
