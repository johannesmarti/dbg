module Spiral (
  fromHub,
) where

import Control.Exception.Base
import qualified Data.Vector as Vec
import qualified Data.Map as Map

import Label
import LabeledGraph

data Spiral a = Spiral {
  word   :: Vec.Vector Label,
  hub    :: Vec.Vector a,
  spokes :: Vec.Vector (Map.Map a Int)
} deriving Eq

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
                         Spiral rword rhub
                             (atDistance 1 (spiralOnHub word hub)) where
  rword = Vec.fromList w
  rhub = Vec.fromList hubList
  spiralOnHub = undefined
  atDistance depth h = undefined

