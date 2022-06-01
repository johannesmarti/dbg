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
improvedSearch size wrappedGraph wordToRel cutoff = undefined
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

type ArcConsResult a = Maybe (HomomorphismTree a, Restrictions a)

snipRMap :: Ord a => RestrictionMap a -> RestrictionMap a
snipRMap = M.mapKeys tail

snip :: Ord a => Restrictions a -> Restrictions a
snip (Restrictions fo ba) = Restrictions (snipRMap fo) (snipRMap ba)

zoSplitRMap :: Ord a => RestrictionMap a -> (RestrictionMap a, RestrictionMap a)
zoSplitRMap = fmap snipRMap . M.partitionWithKey (\k _ -> head k == Zero)

zoSplit :: Ord a => Restrictions a -> (Restrictions a, Restrictions a)
zoSplit (Restrictions f b) = (Restrictions fz bz, Restrictions fo bo) where
  (fz,fo) = zoSplitRMap f
  (bz,bo) = zoSplitRMap b

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

oneArcCons :: Size -> LBitGraph -> Restrictions Node
              -> HomomorphismTree Node -> ArcConsResult Node
oneArcCons size lbg changes (Branch zeroT oneT) = let
    worker (Branch zeroT oneT) env = let
        (zzPred,ozPred) = zoSplit (zeroPredecessors env)
        (zoPred,ooPred) = zoSplit ( onePredecessors env)
        (zSucc,oSucc) = zoSplit (successors env)
        l = localLabel env
      in fuse (worker zeroT (zzPred,zoPred) (l,zSucc))
              (worker  oneT (ozPred,ooPred) (l,oSucc))
{-
    worker (Open nMap pList) (changedOPred, changedZPred)
                             (sl,changedSucc) = undefined
    worker (Closed a) _ _ = Just (Closed a, M.empty)-}
    (epsilonZPred,epsilonOPred) = zoSplit changes
    (zzPred,ozPred) = zoSplit epsilonZPred
    (zoPred,ooPred) = zoSplit epsilonOPred
  in undefined {-fuse (worker zeroT (zzPred,zoPred) (Zero,changedNecLists))
          (worker  oneT (ozPred,ooPred) (One ,changedNecLists))-}
