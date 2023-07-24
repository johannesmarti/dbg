module Plans.Spiral (
  fromHub,
  prettySpiral,
) where

import Control.Exception.Base
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Set.Extra as SE
import Data.List (intercalate, sortOn)

import Data.Label
import Graphs.LabeledGraphInterface
import Plans.Spoke

{- It might not be that good to have Spokes use a list-based, instead of
Set-based, interface. Below I use a lot of Set-based code in the implementation
in from Hub, which interacts awkwardly with the interface form Spokes. -}
data Spiral a = Spiral {
  word   :: V.Vector Label,
  spokes :: V.Vector (Spoke a)
} deriving Show

allIndices :: V.Vector a -> [Int]
allIndices v = [0 .. V.length v - 1]

isCoherent :: Ord a => Spiral a -> Bool
isCoherent (Spiral w s) = V.length w == V.length s

hub :: Spiral a -> [a]
hub = map Plans.Spoke.hub . V.toList . spokes

hubIsConnected :: Ord a => LabeledGraphInterface g a -> g
                           -> [Label] -> [a] -> Bool
hubIsConnected gi g word hub = worker word hub where
  next [] = head hub
  next (c:_) = c
  worker [] [] = True
  worker (l:ls) (c:cs) = hasArc gi g l (c, next cs) && worker ls cs
  worker _ _ = False

fromHub :: Ord a => Ord a => LabeledGraphInterface g a -> g
                             -> [Label] -> [a] -> Spiral a
fromHub gi g w hubList = assert (length w == length hubList) $ 
                         assert (hubIsConnected gi g w hubList) $
                         assert (isCoherent spiral) $
                         spiral where
  spiral = Spiral rword (atDistance 1 initialGenerator spokesAtHub)
  rword = V.fromList w
  hubVec = V.fromList hubList
  spokesAtHub = V.fromList (map singletonSpoke hubList)
  sz = V.length rword
  prevIndex i = (sz + i - 1) `mod` sz
  nextIndex i = (i + 1) `mod` sz
  initialGenerator = V.fromList (map Set.singleton hubList)
  --atDistance :: Int -> V.Vector (Set.Set a) -> V.Vector (Spoke a) -> V.Vector (Spoke a)
  atDistance distance generator spokesAccum =
    if all Set.null generator
      then spokesAccum
      else let newGenerator = V.generate sz (\i -> let
                   pi = prevIndex i
                   l = rword V.! pi
                   alreadyThere = Set.fromList $ Plans.Spoke.nodes (spokesAccum V.! i)
                   -- probabely it would be more efficient to already remove
                   -- the elements from alreadyThere from the successors before
                   -- we do the concatMap
                   allCandidates = SE.concatMap (successors gi g l)
                                     (generator V.! pi)
                 in allCandidates Set.\\ alreadyThere)
               newSpokes = V.generate sz (\i ->
                    merge
                      [(newNode,distance) | newNode <- Set.toList (newGenerator V.! i)]
                      (spokesAccum V.! i))
               merge alist spoke = foldr (uncurry Plans.Spoke.insert) spoke alist
             in atDistance (distance + 1) newGenerator newSpokes

prettySpiral :: (a -> String) -> Spiral a -> [String]
prettySpiral nodePrinter (Spiral w sps) =
  concatMap forIndex (allIndices w) where
    prettyPair (k,n) = "(" ++ nodePrinter k ++ ": " ++ show n ++ ")"
    sortedMap sp = sortOn snd $ points sp
    prettyMap sp = "{" ++ intercalate " " (map prettyPair $ sortedMap sp) ++ "}"
    forIndex i = [prettyMap (sps V.! i), labelToSymbol (w V.! i) ++ "=>"]
