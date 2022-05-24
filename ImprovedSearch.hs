module ImprovedSearch (

) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Label
import CommonLGraphTypes
import BitGraph

type RestrictionMap a = M.Map [Label] (S.Set a)

data HomomorphismTree a = Branch {
    zeroSuccessor :: HomomorphismTree a,
    oneSuccessor :: HomomorphismTree a } |
  Open {
    necMap :: RestrictionMap a,
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

type ArcConsResult a = Maybe (HomomorphismTree Node, RestrictionMap Node)

fuse :: ArcConsResult a -> ArcConsResult a -> ArcConsResult a
fuse (Just (zT, zM)) (Just (oT, oM)) = Just (Branch zT oT, fusedMap) where
  ezM = M.mapKeys (Zero:) zM
  eoM = M.mapKeys ( One:) oM
  fusedMap = ezM `M.union` eoM
fuse _ _ = Nothing

oneArcCons :: Size -> LBitGraph -> RestrictionMap Node
              -> HomomorphismTree Node -> ArcConsResult Node
oneArcCons size lbg changedNecList ht =
  worker ht (changedNecList, changedNecList) changedNecList where
    worker (Branch zeroT oneT) (changedOPred, changedZPred)
                               changedSucc = let
        (zzPred',ozPred') = M.partitionWithKey (\k _ -> head k == Zero) changedZPred
        (zoPred',ooPred') = M.partitionWithKey (\k _ -> head k == Zero) changedOPred
        (zSucc',oSucc') = M.partitionWithKey (\k _ -> head k == Zero) changedSucc
        snip = M.mapKeys tail
        (zzPred,ozPred) = (snip zzPred', snip ozPred')
        (zoPred,ooPred) = (snip zoPred', snip ooPred')
        (zSucc,oSucc) = (snip zSucc',snip oSucc')
      in fuse (worker zeroT (zzPred,zoPred) zSucc)
              (worker  oneT (ozPred,ooPred) oSucc)
    worker (Open nMap pList) (changedOPred, changedZPred)
                             changedSucc = undefined
    worker (Closed a) _ _ = Just (Closed a, M.empty)
