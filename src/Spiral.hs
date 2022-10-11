module Spiral (
  fromHub,
) where

import Control.Exception.Base
import qualified Data.Vector as Vec
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Set.Extra as SE

import Label
import LabeledGraph

data Spiral a = Spiral {
  word   :: Vec.Vector Label,
  hub    :: Vec.Vector a,
  spokes :: Vec.Vector (Map.Map a Int)
} deriving (Eq, Show)

allIndices :: Vec.Vector a -> [Int]
allIndices v = [0 .. Vec.length v - 1]

isCoherent :: Ord a => Spiral a -> Bool
isCoherent (Spiral w h s) =
  Vec.length w == Vec.length h &&
  Vec.length w == Vec.length s &&
  all (\i -> (s Vec.! i)  Map.! (h Vec.! i) == 0) (allIndices h)

hubIsConnected :: Ord a => LabeledGraphI g a -> g
                           -> [Label] -> [a] -> Bool
hubIsConnected gi g word hub = worker word hub where
  next [] = head hub
  next (c:_) = c
  worker [] [] = True
  worker (l:ls) (c:cs) = hasArc gi g l (c, next cs) && worker ls cs
  worker _ _ = False

fromHub :: Ord a => Ord a => LabeledGraphI g a -> g
                             -> [Label] -> [a] -> Spiral a
fromHub gi g w hubList = assert (length w == length hubList) $ 
                         assert (hubIsConnected gi g w hubList) $
                         assert (isCoherent spiral) $
                         spiral where
  spiral = Spiral rword rhub (atDistance 1 initialGenerator spokesAtHub)
  rword = Vec.fromList w
  rhub = Vec.fromList hubList
  sz = Vec.length rword
  prevIndex i = (sz + i - 1) `mod` sz
  nextIndex i = (i + 1) `mod` sz
  spokesAtHub = Vec.generate sz (\i -> Map.singleton (rhub Vec.! i) 0)
  initialGenerator = Vec.generate sz (\i -> Set.singleton (rhub Vec.! i))
  atDistance distance generator spokesAccum =
    if all Set.null generator
      then spokesAccum
      else let a = undefined
               newGenerator = Vec.generate sz (\i -> let
                   pi = prevIndex i
                   l = rword Vec.! pi
                   alreadyThere = Map.keysSet (spokesAccum Vec.! i)
                   -- probabely it would be more efficient to already remove
                   -- the elements from alreadyThere from the successors before
                   -- we do the concatMap
                   allCandidates = SE.concatMap (successors gi g l)
                                     (generator Vec.! pi)
                 in allCandidates Set.\\ alreadyThere)
               newSpokes = Vec.generate sz (\i ->
                    Map.union
                      (spokesAccum Vec.! i)
                      (Map.fromSet (const distance) (newGenerator Vec.! i)))
             in atDistance (distance + 1) newGenerator newSpokes

