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

snip :: Ord a => RestrictionMap a -> RestrictionMap a
snip = M.mapKeys tail

zoSplit :: Ord a => RestrictionMap a -> (RestrictionMap a, RestrictionMap a)
zoSplit = fmap snip . M.partitionWithKey (\k _ -> head k == Zero)

fuse :: ArcConsResult a -> ArcConsResult a -> ArcConsResult a
fuse (Just (zT, zM)) (Just (oT, oM)) = Just (Branch zT oT, fusedMap) where
  ezM = M.mapKeys (Zero:) zM
  eoM = M.mapKeys ( One:) oM
  fusedMap = ezM `M.union` eoM
fuse _ _ = Nothing

oneArcCons :: Size -> LBitGraph -> HomomorphismTree Node
              -> RestrictionMap Node -> ArcConsResult Node
oneArcCons size lbg ht changedNecLists = let
    worker (Branch zeroT oneT) (changedOPred, changedZPred) = let
        (zzPred,ozPred) = zoSplit changedZPred
        (zoPred,ooPred) = zoSplit changedOPred
      in fuse (worker zeroT (zzPred,zoPred))
              (worker  oneT (ozPred,ooPred))
    worker (Open nMap pList) (changedOPred, changedZPred) = undefined
    worker (Closed a) _ = Just (Closed a, M.empty)
    (epsilonZPred,epsilonOPred) = zoSplit changedNecLists
  in worker ht (epsilonZPred,epsilonOPred)
