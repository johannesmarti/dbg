module Spiral (
  fromHub,
) where

import Control.Exception.Base
import qualified Data.Vector as Vec
import qualified Data.Map as Map

import Label
import LabeledGraph

data Spiral a = Spiral {
  word   :: Vec.Vector Label
  hub    :: Vec.Vector a
  spokes :: Vec.Vector (Map.Map a Int)
} deriving Eq

isCoherent :: Ord a => Spiral a -> Bool
isCoherent (Spiral w h s) =
  Vec.length w == Vec.length h &&
  Vec.length w == Vec.length s &&
  all (\n -> (distances r) Map.! n == 0) hub

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
fromHub gi g word hub = assert (length word == length hub) $ 
                        assert (hubIsConnected gi g word hub) $ undefined
                        atDistance 1 (spiralOnHub word hub) where
  spiralOnHub
  atDistance depth Spiral = undefined

