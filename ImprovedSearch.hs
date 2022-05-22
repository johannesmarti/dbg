module ImprovedSearch (

) where

import qualified Data.Set as S

import Label

data HomomorphismTree a = Branch {
    zeroSuccessor :: HomomorphismTree a,
    oneSuccessor :: HomomorphismTree a } |
  Open {
    cyclicWord :: [Label],
    necList :: S.Set a,
    posList :: S.Set a} |
  Closed a
    deriving Show

improvedSearch :: Ord a => LabeledGraphI g a -> g -> ([Label] ->) ()
