module ImprovedSearch (

) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Label
import CommonLGraphTypes
import BitGraph

type RestrictionMap a = M.Map [Label] (S.Set a)
type CycleMap a = M.Map [Label] (S.Set a)

data HomomorphismTree a =
  Branch {
    zeroSuccessor :: HomomorphismTree a,
    oneSuccessor :: HomomorphismTree a } |
  Open {
    necMap :: CycleMap a,
    posList :: S.Set a} |
  Closed a
    deriving Show

{- Keep in mind that there are more than one cyclicWord that a node might be on! -}

improvedSearch :: Size -> LBitGraph -> ([Label] -> BitGraph) -> Int -> Maybe (HomomorphismTree Node)
improvedSearch s wrappedGraph wordToRel cutoff = undefined
{-
  expand
  choose node to fix
  fix and check arc cons on posLists
  this induces changes to necLists
  promote changes to necLists among circles
  expand
-}

data Restrictions a = Restrictions {
  forwardRestrictions :: RestrictionMap a,
  backwardRestrictions :: RestrictionMap a
} deriving Show

noRestrictions :: Restrictions a
noRestrictions = Restrictions M.empty M.empty

type ArcConsResult a = Maybe (HomomorphismTree a, Restrictions a)

snipRMap :: Ord a => RestrictionMap a -> RestrictionMap a
snipRMap = M.mapKeys tail

snip :: Ord a => Restrictions a -> Restrictions a
snip (Restrictions fo ba) = Restrictions (snipRMap fo) (snipRMap ba)

zoSplit :: Ord a => RestrictionMap a -> (RestrictionMap a, RestrictionMap a)
zoSplit = fmap snipRMap . M.partitionWithKey (\k _ -> head k == Zero)

{-
zoSplit :: Ord a => Restrictions a -> (Restrictions a, Restrictions a)
zoSplit (Restrictions f b) = (Restrictions fz bz, Restrictions fo bo) where
  (fz,fo) = zoSplitRMap f
  (bz,bo) = zoSplitRMap b
-}

fuseRMap :: RestrictionMap a -> RestrictionMap a -> RestrictionMap a
fuseRMap zM oM = ezM `M.union` eoM where
  ezM = M.mapKeys (Zero:) zM
  eoM = M.mapKeys ( One:) oM

fuseRestrictions :: Restrictions a -> Restrictions a -> Restrictions a
fuseRestrictions (Restrictions fz bz) (Restrictions fo bo) =
  Restrictions f b where
    f = fuseRMap fz fo
    b = fuseRMap bz bo

fuse :: ArcConsResult a -> ArcConsResult a -> ArcConsResult a
fuse (Just (zT, zR)) (Just (oT, oR)) = Just (Branch zT oT, fusedRestr) where
  fusedRestr = fuseRestrictions zR oR
fuse _ _ = Nothing

data Environment a = Environment {
  zeroPredecessors :: a,
   onePredecessors :: a,
  localLabel :: Label,
  successors :: a
} deriving Show

compatibleWithRestrEnv :: Size -> LBitGraph
                          -> Environment (RestrictionMap Node) -> Bool
compatibleWithRestrEnv a env = undefined

subtreeArcCons :: Size -> LBitGraph -> HomomorphismTree Node
                  -> Environment (RestrictionMap Node) -> ArcConsResult Node
subtreeArcCons s lbg (Branch zeroT oneT) env = let
    {- Here we could check whether the environment is empty, in which case
       we could omit descending into the current subtree -}
    (zzPred,ozPred) = zoSplit (zeroPredecessors env)
    (zoPred,ooPred) = zoSplit ( onePredecessors env)
    (zSucc,oSucc) = zoSplit (successors env)
    l = localLabel env
    zenv = Environment zzPred zoPred l zSucc
    oenv = Environment ozPred ooPred l oSucc
  in fuse (subtreeArcCons s lbg zeroT zenv)
          (subtreeArcCons s lbg  oneT oenv)
subtreeArcCons s lbg (Closed a) env =
  if compatibleWithRestrEnv s lbg a env
    then Just (Closed a, noRestrictions)
    else Nothing

oneArcCons :: Size -> LBitGraph -> Restrictions Node
              -> HomomorphismTree Node -> ArcConsResult Node
oneArcCons s lbg changes (Branch zeroT oneT) = let
    (epsilonZPred,epsilonOPred) = zoSplit (forwardRestrictions changes)
    (zzPred,ozPred) = zoSplit epsilonZPred
    (zoPred,ooPred) = zoSplit epsilonOPred
    bwRestr = backwardRestrictions changes
  in fuse (subtreeArcCons s lbg zeroT (Environment zzPred zoPred Zero bwRestr))
          (subtreeArcCons s lbg  oneT (Environment ozPred ooPred  One bwRestr))
