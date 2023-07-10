module Main (
  main
) where

import System.Environment 
import Data.Set as Set

import Report
import Examples.Patterns
import Label
import LabeledGraph
import DeBruijnGraph


import qualified SmartSearch as SS


main :: IO ()

--(gi, g) = (dbgI,dbg 4)
--(gi, g) = (alloc3I,alloc3)
--(gi, g) = (big5I,big5)
--(gi, g) = (force9dI,force9d)
(gi, g) = (force3dI,force3d)
--(gi, g) = (b1ef5I,b1ef5)
--(gi, g) = (specialUnfoldI,specialUnfold)
--(gi, g) = (biggestI,biggest)
--(gi, g) = (hamburgerI,hamburger)
main = do
  easyCayleyReport gi g

