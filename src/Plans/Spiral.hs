module Plans.Spiral (
  Spiral,
  spiralPredecessor,
  fromHub,
  spiralsForWord,
  generatedSubspirals,
  prettySpiral,
) where

import Control.Exception.Base (assert)
import qualified Data.Vector as V
import qualified Data.Set as Set
import qualified Data.Set.Extra as SE
import Data.List (intercalate, sortOn, nub)

import Data.Label
import Graphs.LabeledGraphInterface
import GraphTools.RelationCache
import GraphTools.PathTree
import qualified Plans.Spoke as Spoke

{- It might not be that good to have Spokes use a list-based, instead of
Set-based, interface. Below I use a lot of Set-based code in the implementation
in from Hub, which interacts awkwardly with the interface form Spokes. -}
data Spiral a = Spiral {
  word           :: V.Vector Label,
  spokes         :: V.Vector (Spoke.Spoke a)
} deriving (Show, Eq)

allIndices :: V.Vector a -> [Int]
allIndices v = [0 .. V.length v - 1]

isCoherent :: Spiral a -> Bool
isCoherent (Spiral w s) = V.length w == V.length s

circumfence :: Spiral a -> Int
circumfence sp = V.length (word sp)

spiralPredecessors :: Ord a => MapFunction a -> Spiral a -> Int -> a
                               -> Set.Set a
spiralPredecessors pred sp ix node = let
    sz = circumfence sp
    prevIndex i = (sz + i - 1) `mod` sz
    previous = prevIndex ix
    l = word sp V.! previous
    spoks = spokes sp
    myDistance = distanceOf (spokes V.! ix) node
    candidates = Set.fromList $ pointsAtDistanceList (spokes V.! previous)
  in assert (myDistance > 0) $ candidates `Set.intersect` pred l node

hubList :: Spiral a -> [a]
hubList = map Spoke.hub . V.toList . spokes

hubIsConnected :: LabeledGraphInterface g a -> g -> [Label] -> [a] -> Bool
hubIsConnected gi g word hub = worker word hub where
  next [] = head hub
  next (c:_) = c
  worker [] [] = True
  worker (l:ls) (c:cs) = hasArc gi g l (c, next cs) && worker ls cs
  worker _ _ = False

fromHub :: Ord a => LabeledGraphInterface g a -> g -> [Label] -> [a] -> Spiral a
fromHub gi g w hubList = assert (length w == length hubList) $ 
                         assert (hubIsConnected gi g w hubList) $
                         assert (isCoherent spiral) $
                         spiral where
  spiral = Spiral rword (atDistance 1 initialGenerator spokesAtHub)
  rword = V.fromList w
  spokesAtHub = V.fromList (map Spoke.singleton hubList)
  sz = V.length rword
  prevIndex i = (sz + i - 1) `mod` sz
  --nextIndex i = (i + 1) `mod` sz
  initialGenerator = V.fromList (map Set.singleton hubList)
  --atDistance :: Int -> V.Vector (Set.Set a) -> V.Vector (Spoke a) -> V.Vector (Spoke a)
  atDistance distance generator spokesAccum =
    if all Set.null generator
      then spokesAccum
      else let newGenerator = V.generate sz (\i -> let
                   prevI = prevIndex i
                   l = rword V.! prevI
                   alreadyThere = Spoke.nodes (spokesAccum V.! i)
                   -- probabely it would be more efficient to already remove
                   -- the elements from alreadyThere from the successors before
                   -- we do the concatMap
                   allCandidates = SE.concatMap (successors gi g l)
                                     (generator V.! prevI)
                 in allCandidates Set.\\ alreadyThere)
               newSpokes = V.generate sz (\i ->
                    merge
                      [(newNode,distance) | newNode <- Set.toList (newGenerator V.! i)]
                      (spokesAccum V.! i))
               merge alist spoke = foldr (uncurry Spoke.insert) spoke alist
             in atDistance (distance + 1) newGenerator newSpokes

{-
 FIXME: It is annoying that this function needs both the LabedGraph and the
RelationCache as arguments. It should work with just the relation cache, as it
contains all the relevant information. But then there is probabely a completely
different arrangement of the information that makes this all much cleaner.
-}
spiralsForWord :: Ord x => LabeledGraphInterface g x -> g
                           -> RelationCache r x -> [Label] -> [Spiral x]
spiralsForWord gi g rc w = map getSpiral cycles where
  cycles = cyclesOfWord rc w
  getSpiral cycle = fromHub gi g w cycle

justHub :: Ord x => Spiral x -> Spiral x
justHub (Spiral w sps) =
  Spiral w (V.map (\sp -> Spoke.singleton (Spoke.hub sp)) sps)

generatedSubspirals :: Ord a => MapFunction a -> Spiral a
                                -> V.Vector (Set.Set a) -> [V.Vector (Set.Set a)]
-- make sure to call nub in the end
generatedSubspirals pred s generators = let
    w = word s
    worker newlyAdded accumulated =
      if all Set.null newlyAdded then return accumulated
      else undefined
           -- for each set in newlyAdded and element in the set consider it's predecessors under the label on the previous step. 
           -- intersect this set of predecessors with the set at the predecessor on the spiral of all nodes that have a distance one-less than the node.
           -- pick a random node from the intersection and add it to the accumlator, if it is not of distance 1 then also add it to some set of newlyAddeds
  in assert (length w == length generators) $
       nub $ worker generators (justHub s)

prettySpiral :: (a -> String) -> Spiral a -> [String]
prettySpiral nodePrinter (Spiral w sps) =
  concatMap forIndex (allIndices w) where
    prettyPair (k,n) = "(" ++ nodePrinter k ++ ": " ++ show n ++ ")"
    sortedMap sp = sortOn snd $ Spoke.pointsAList sp
    prettyMap sp = "{" ++ intercalate " " (map prettyPair $ sortedMap sp) ++ "}"
    forIndex i = [prettyMap (sps V.! i), labelToSymbol (w V.! i) ++ "=>"]
