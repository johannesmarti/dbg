module Spiral (

) where

import Control.Exception.Base
import qualified Data.Vector as Vec
import qualified Data.Map as Map

import Label

data Spiral a = Spiral {
  word :: [Label],
  rays :: Vec.Vector (Ray a)
}

data Ray a = Ray {
  base :: a,
  distances :: Map.Map a Int
}

isCoherent :: Ord a => Spiral a -> Bool
isCoherent (Spiral w ds) =
  length w == Vec.length ds &&
  all (\r -> (distances r) Map.! (base r) == 0) ds
