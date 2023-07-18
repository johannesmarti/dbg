module HomomorphismSearch.AllocateWords (
  isPossibleValue,
  firstLevelToAllocate,
) where

import Data.ListWord
import Data.Label
import Graphs.BitGraph

isPossibleValue :: Size -> ([Label] -> BitGraph) -> [Label] -> Node -> Bool
isPossibleValue size relOfWord word node =
  all (\v -> hasBitForArc size (relOfWord word) (node,v)) (nodes size) &&
  all (\i -> hasBitForArc size (relOfWord i) (node,node)) (repeatingInits word)

canAllocateAtLevel :: Size -> ([Label] -> BitGraph) -> Int -> Bool
canAllocateAtLevel size relOfWord level = all hasGoodNode allWords where
  allWords = allWordsOfLength labelsList level
  hasGoodNode word = any (isPossibleValue size relOfWord word) (nodes size)

firstLevelToAllocate :: Size -> ([Label] -> BitGraph) -> Int
firstLevelToAllocate size relOfWord = search 1 where
  search i = if canAllocateAtLevel size relOfWord i
               then i
               else search (i + 1)
