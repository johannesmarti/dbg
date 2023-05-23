module WordMap.Coalgebraic (
  WordMap,
  empty,
  combine,
  WordMap.Coalgebraic.lookup,
  insert,
  delete,
) where

import Label
import WordTree

type WordMap x = WordTree (Maybe x)

empty :: WordMap x
empty = wordTreeFromGenerator gen where
  gen = WordTreeGenerator Nothing (const Nothing) (const Nothing)

combine :: x -> WordMap x -> WordMap x -> WordMap x
combine lbl left right = WordTreeNode (Just lbl) left right

lookup :: [Label] -> WordMap x -> Maybe x
lookup key wt = labelOfWord wt key

insert :: [Label] -> x -> WordMap x -> WordMap x
insert key value wt = setWord (Just value) key wt

delete :: [Label] -> WordMap x -> WordMap x
delete key wt = setWord Nothing key wt
