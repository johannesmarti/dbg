module Data.WordTree (
  WordTreeGenerator(..),
  WordTree(..),
  wordTreeFromGenerator,
  wordTreeFromFunction,
  subtreeOfWord,
  labelOfWord,
  updateWord,
  updateWordM,
  setWord,
) where

import Data.Label

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

wordTreeFromGenerator :: WordTreeGenerator d -> WordTree d
wordTreeFromGenerator di = worker (start di) where
  worker d = WordTreeNode d
               (worker (appendZero di d))
               (worker (appendOne  di d))

wordTreeFromFunction :: ([Label] -> d) -> WordTree d
wordTreeFromFunction fct = initNode [] where
  initNode word = WordTreeNode (fct word)
                        (initNode (word ++ [Zero]))
                        (initNode (word ++ [One]))

subtreeOfWord :: WordTree d -> [Label] -> WordTree d
subtreeOfWord wt [] = wt
subtreeOfWord wt (Zero:rest) = subtreeOfWord (zeroSucc wt) rest
subtreeOfWord wt (One:rest)  = subtreeOfWord ( oneSucc wt) rest

labelOfWord :: WordTree d -> [Label] -> d
labelOfWord wt = label . subtreeOfWord wt

updateWord :: (d -> d) -> [Label] -> WordTree d -> WordTree d
updateWord updater [] (WordTreeNode l zs os) =
  WordTreeNode (updater l) zs os
updateWord updater (Zero:rest) (WordTreeNode l zs os) =
  WordTreeNode l (updateWord updater rest zs) os
updateWord updater (One:rest) (WordTreeNode l zs os) =
  WordTreeNode l zs (updateWord updater rest os)

updateWordM :: Monad m => (d -> m d) -> [Label] -> WordTree d -> m (WordTree d)
updateWordM updater [] (WordTreeNode l zs os) = do
  l' <- updater l
  return $ WordTreeNode l' zs os
updateWordM updater (Zero:rest) (WordTreeNode l zs os) = do
  zs' <- updateWordM updater rest zs
  return $ WordTreeNode l zs' os
updateWordM updater (One:rest) (WordTreeNode l zs os) = do
  os' <- updateWordM updater rest os
  return $ WordTreeNode l zs os'

setWord :: d -> [Label] -> WordTree d -> WordTree d
setWord value word wt = updateWord (const value) word wt
