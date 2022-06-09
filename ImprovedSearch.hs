module ImprovedSearch (

) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Label
import CommonLGraphTypes
import BitGraph
import LabeledGraph as LG

type RestrictionMap a = M.Map [Label] (S.Set a)
type CycleMap a = M.Map [Label] (S.Set a)

removeFromCycleMap :: Ord a => S.Set a -> CycleMap a -> Maybe (CycleMap a)
removeFromCycleMap toRemove cm = let
    newMap = M.map (S.\\ toRemove) cm
    nonEmpty set = not (null set)
  in if all nonEmpty (M.elems cm)
       then Just newMap
       else Nothing

data HomomorphismTree a =
  Branch {
    zeroSuccessor :: HomomorphismTree a,
    oneSuccessor :: HomomorphismTree a } |
  Open {
    cycleMap :: CycleMap a,
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

restrictionsAtLeaf :: Ord a => (S.Set a, S.Set a) -> (S.Set a, S.Set a)
                               -> Restrictions a
restrictionsAtLeaf (zPos,zout) (oPos,oout) = Restrictions fwd bwd where
  oFMap = if null zout then M.empty else M.singleton [] zPos
  zFMap = if null oout then M.empty else M.singleton [] oPos
  fwd = oFMap `fuseRMap` zFMap
  bout = (zout S.\\ oPos) `S.union` (oout S.\\ zPos)
  bwd = if null bout then M.empty else M.singleton [] (zPos `S.union` oPos)

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
  localSuccessors :: a
} deriving Show

type RestrEnv a = Environment (RestrictionMap a)

splitEnv :: Ord a => RestrEnv a -> (RestrEnv a, RestrEnv a)
splitEnv env = (zenv, oenv) where
  (zzPred,ozPred) = zoSplit (zeroPredecessors env)
  (zoPred,ooPred) = zoSplit ( onePredecessors env)
  (zSucc,oSucc) = zoSplit (localSuccessors env)
  l = localLabel env
  zenv = Environment zzPred zoPred l zSucc
  oenv = Environment ozPred ooPred l oSucc

compatibleWithRestrEnv :: Size -> LBitGraph -> RestrEnv Node -> Node -> Bool
compatibleWithRestrEnv s lbg env a = res where
  gi = lBitGraphI s
  isPredecessor l v = hasArc gi lbg l (v,a)
  isSuccessor v = hasArc gi lbg (localLabel env) (a,v)
  hasPredecessor l set = any (isPredecessor l) set
  hasSuccessor set = any isSuccessor set
  zPredComp = all (hasPredecessor Zero) (zeroPredecessors env)
  oPredComp = all (hasPredecessor  One) ( onePredecessors env)
  succComp = all hasSuccessor (localSuccessors env)
  res = zPredComp && oPredComp && succComp

type LeafData a = (CycleMap a, S.Set a)

updateOpen :: Size -> LBitGraph -> RestrEnv Node -> LeafData Node
              -> Maybe (LeafData Node, S.Set Node)
updateOpen s lbg env (cycleM, posL) = do
  let (posL',out) = S.partition (compatibleWithRestrEnv s lbg env) posL
  cycleM' <- removeFromCycleMap out cycleM
  return ((cycleM', posL'), out)

updateClosed :: Size -> LBitGraph -> RestrEnv Node -> Node
                -> Maybe (S.Set Node, S.Set Node)
updateClosed s lbg env n =
  if compatibleWithRestrEnv s lbg env n
    then Just (S.singleton n, S.empty)
    else Nothing

subtreeArcCons :: Size -> LBitGraph -> HomomorphismTree Node
                  -> RestrEnv Node -> ArcConsResult Node

subtreeArcCons s lbg (Branch (Open zCMap zPosList)
                             (Open oCMap oPosList)) env = do
  let (zenv,oenv) = splitEnv env
  ((zCMap', zPosList'), zout) <- updateOpen s lbg zenv (zCMap, zPosList)
  ((oCMap', oPosList'), oout) <- updateOpen s lbg oenv (oCMap, oPosList)
  let restrictions = restrictionsAtLeaf (zPosList', zout) (oPosList', oout)
  return (Branch (Open zCMap' zPosList')
                 (Open oCMap' oPosList'), restrictions)

subtreeArcCons s lbg (Branch (Open zCMap zPosList)
                             (Closed c)) env = do
  let (zenv,oenv) = splitEnv env
  ((zCMap', zPosList'), zout) <- updateOpen s lbg zenv (zCMap, zPosList)
  (oPosList', oout) <- updateClosed s lbg oenv c
  let restrictions = restrictionsAtLeaf (zPosList', zout) (oPosList', oout)
  return (Branch (Open zCMap' zPosList')
                 (Closed c), restrictions)

subtreeArcCons s lbg (Branch (Closed c)
                             (Open oCMap oPosList)) env = do
  let (zenv,oenv) = splitEnv env
  (zPosList', zout) <- updateClosed s lbg zenv c
  ((oCMap', oPosList'), oout) <- updateOpen s lbg oenv (oCMap, oPosList)
  let restrictions = restrictionsAtLeaf (zPosList', zout) (oPosList', oout)
  return (Branch (Closed c)
                 (Open oCMap' oPosList'), restrictions)


subtreeArcCons s lbg (Branch zeroT oneT) env = let
    {- Here we could check whether the environment is empty, in which case
       we could omit descending into the current subtree -}
    (zenv,oenv) = splitEnv env
  in fuse (subtreeArcCons s lbg zeroT zenv)
          (subtreeArcCons s lbg  oneT oenv)

subtreeArcCons s lbg (Closed a) env =
  if compatibleWithRestrEnv s lbg env a
    then Just (Closed a, noRestrictions)
    else Nothing

oneArcCons :: Size -> LBitGraph -> Restrictions Node
              -> HomomorphismTree Node -> ArcConsResult Node
{-
oneArcCons s lbg changes (Branch (Open zCMap zPosList)
                                 (Open oCMap oPosList)) = let
    (epsilonZPred,epsilonOPred) = zoSplit (forwardRestrictions changes)
    (zzPred,ozPred) = zoSplit epsilonZPred
    (zoPred,ooPred) = zoSplit epsilonOPred
    bwRestr = backwardRestrictions changes
  in fuse (subtreeArcCons s lbg zeroT (Environment zzPred zoPred Zero bwRestr))
          (subtreeArcCons s lbg  oneT (Environment ozPred ooPred  One bwRestr))
-}

oneArcCons s lbg changes (Branch zeroT oneT) = let
    (epsilonZPred,epsilonOPred) = zoSplit (forwardRestrictions changes)
    (zzPred,ozPred) = zoSplit epsilonZPred
    (zoPred,ooPred) = zoSplit epsilonOPred
    bwRestr = backwardRestrictions changes
  in fuse (subtreeArcCons s lbg zeroT (Environment zzPred zoPred Zero bwRestr))
          (subtreeArcCons s lbg  oneT (Environment ozPred ooPred  One bwRestr))
