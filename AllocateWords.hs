module AllocateWords (
  isPossibleValue,
) where

import Word
import Label
import BitGraph

isPossibleValue :: Size -> ([Label] -> BitGraph) -> [Label] -> Node -> Bool
isPossibleValue size relOfWord word node =
  all (\v -> hasBitForArc size (relOfWord word) (node,v)) (nodes size) &&
  all (\i -> hasBitForArc size (relOfWord i) (node,node)) (repeatingInits word)

canAllocateAtLevel :: Size -> ([Label] -> BitGraph) -> Int -> Bool
canAllocateAtLevel size relOfWord level = all hasGoodNode allWords where
  allWords = allWordsOfLength labelsList level
  hasGoodNode word = any (isPossibleValue size relOfWord word) (nodes size)
