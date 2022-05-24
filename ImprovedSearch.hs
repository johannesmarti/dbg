module ImprovedSearch (

) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Label
import CommonLGraphTypes
import BitGraph

data HomomorphismTree a = Branch {
    zeroSuccessor :: HomomorphismTree a,
    oneSuccessor :: HomomorphismTree a } |
  Open {
    necMap :: M.Map [Label] (S.Set a),
    posList :: S.Set a} |
  Closed a
    deriving Show

{- Keep in mind that there are more than one cyclicWord that a node might be on! -}

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

oneArcCons :: Size -> LBitGraph -> M.Map [Label] (S.Set Node)
              -> HomomorphismTree Node
              -> Maybe (HomomorphismTree Node, M.Map [Label] (S.Set Node))
oneArcCons size lbg changedNecLists ht = worker ht 0 let
    worker (Branch zeroT oneT) listOfChanged = undefined
  in undefined
