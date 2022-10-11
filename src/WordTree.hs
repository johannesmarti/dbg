module WordTree (
  WordTreeGenerator(..),
  WordTree(..),
  wordTree,
  labelOfWord,
  allWordsUntil,
  allWordsWithout,
) where

import Label

data WordTreeGenerator d = WordTreeGenerator {
  start :: d,
  appendZero :: d -> d,
  appendOne :: d -> d
}

data WordTree d = WordTreeNode {
  label    :: d,
  zeroSucc :: WordTree d,
  oneSucc  :: WordTree d
}

wordTree :: WordTreeGenerator d -> WordTree d
wordTree di = worker (start di) where
  worker d = WordTreeNode d
               (worker (appendZero di d))
               (worker (appendOne  di d))

subtreeOfWord :: WordTree d -> [Label] -> WordTree d
subtreeOfWord wt [] = wt
subtreeOfWord wt (Zero:rest) = subtreeOfWord (zeroSucc wt) rest
subtreeOfWord wt (One:rest)  = subtreeOfWord ( oneSucc wt) rest

labelOfWord :: WordTree d -> [Label] -> d
labelOfWord wt = label . subtreeOfWord wt

-- This function could maybe be made quicker by using Sequence.
allWordsUntil :: WordTree d -> (d -> Bool) -> [([Label],WordTree d)]
allWordsUntil wt untilCondition = generateAllWords [([],wt)] where
  generateAllWords [] = []
  generateAllWords ((nextWord, nextTree):rest) =
    if untilCondition (label nextTree)
      then (reverse nextWord,nextTree) : generateAllWords rest
      else (reverse nextWord,nextTree) :
                 (generateAllWords (rest ++ [(Zero:nextWord,zeroSucc nextTree),
                                             (One:nextWord,oneSucc nextTree)]))

-- This function could maybe be made quicker by using Sequence.
allWordsWithout :: WordTree d -> (d -> Bool) -> [([Label],WordTree d)]
allWordsWithout wt withoutCondition = generateAllWords [([], wt)] where
  generateAllWords [] = []
  generateAllWords ((nextWord, nextTree):rest) =
    if withoutCondition (label nextTree)
      then generateAllWords rest
      else (reverse nextWord,nextTree) :
                 (generateAllWords (rest ++ [(Zero:nextWord,zeroSucc nextTree),
                                             (One:nextWord,oneSucc nextTree)]))
