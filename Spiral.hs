module Spiral (

) where

import Control.Exception.Base
import qualified Data.Vector as Vec
import qualified Data.Map as Map

import Label
import LabeledGraph

data Spiral a = Spiral {
  word :: [Label],
  spokes :: Vec.Vector (Spoke a)
} deriving Eq

data Spoke a = Spoke {
  base :: a,
  distances :: Map.Map a Int
} deriving Eq

isCoherent :: Ord a => Spiral a -> Bool
isCoherent (Spiral w ds) =
  length w == Vec.length ds &&
  all (\r -> (distances r) Map.! (base r) == 0) ds

hubIsConnected :: Ord a => LabeledGraphI g a -> g
                            -> [Label] -> [a] -> Bool
hubIsConnected gi g word hub = worker word hub where
  next [] = head hub
  next (c:_) = c
  worker [] [] = True
  worker (l:ls) (c:cs) = hasArc gi g l (c, next cs) && worker ls cs
  worker _ _ = False

fromCore :: Ord a => Ord a => LabeledGraphI g a -> g
                              -> [Label] -> [a] -> Spiral a
fromCore gi g word hub = assert (length word == length hub) $ 
                         assert (hubIsConnected gi g word hub) $ undefined

