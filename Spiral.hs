module Spiral (

) where

import Control.Exception.Base
import qualified Data.Vector as Vec
import qualified Data.Map as Map

import Label
import LabeledGraph

data Spiral a = Spiral {
  word :: [Label],
  rays :: Vec.Vector (Ray a)
} deriving Eq

data Ray a = Ray {
  base :: a,
  distances :: Map.Map a Int
} deriving Eq

isCoherent :: Ord a => Spiral a -> Bool
isCoherent (Spiral w ds) =
  length w == Vec.length ds &&
  all (\r -> (distances r) Map.! (base r) == 0) ds

coreIsConnected :: Ord a => LabeledGraphI g a -> g
                            -> [Label] -> [a] -> Bool
coreIsConnected gi g word core = worker word core where
  next [] = head core
  next (c:_) = c
  worker [] [] = True
  worker (l:ls) (c:cs) = hasArc gi g l (c, next cs) && worker ls cs
  worker _ _ = False

fromCore :: Ord a => Ord a => LabeledGraphI g a -> g
                              -> [Label] -> [a] -> Spiral a
fromCore gi g word core = assert (length word == length core) $ 
                          assert (coreIsConnected gi g word core) $ undefined

