module ImprovedSearch (

) where

import qualified Data.Set as S

import Label
import CommonLGraphTypes
import BitGraph

data HomomorphismTree a = Branch {
    zeroSuccessor :: HomomorphismTree a,
    oneSuccessor :: HomomorphismTree a } |
  Open {
    cyclicWord :: [Label],
    necList :: S.Set a,
    posList :: S.Set a} |
  Closed a
    deriving Show

improvedSearch :: Size -> LBitGraph -> ([Label] -> BitGraph) -> Int -> Maybe (HomomorphismTree Node)
improvedSearch size wrappedGraph wordToRel cutoff = undefined
{-
  expand
  choose node to fix
  fix and check arc cons on posLists
  this induces changes to necLists
  promote changes to necLists among circles
  expand
-}
