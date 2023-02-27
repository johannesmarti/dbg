module Main (
  main
) where

import System.Environment 
import Data.Set as Set

import Report
import Patterns
import Label
import Spiral
import ConstructionGraph
import DeBruijnGraph
import LabeledGraph

import qualified SmartSearch as SS


main :: IO ()

--(gi, g) = (dbgI,dbg 4)
--(gi, g) = (alloc3I,alloc3)
--(gi, g) = (big5I,big5)
--(gi, g) = (force9dI,force9d)
--(gi, g) = (force3dI,force3d)
--(gi, g) = (b1ef5I,b1ef5)
--(gi, g) = (specialUnfoldI,specialUnfold)
--(gi, g) = (biggestI,biggest)
(gi, g) = (hamburgerI,hamburger)
pgi = powerGraphI gi
main = do
  putStr . unlines $ prettyLabeledGraph gi g
  putChar '\n'
  putStr . unlines $ prettyLabeledGraph (converseI gi) g
  putChar '\n'
  print $ SS.searchUpTo 6 gi g
  putChar '\n'
  --putStr . unlines $ prettyBigLabeledGraph (converseI pgi) g 
  --putChar '\n'
  putStr . unlines . (prettyReachability (prettyNode pgi g)) $ universalReachability pgi g (Set.map Set.singleton (domain gi g)) 
  putChar '\n'
  easySpiralReport 8 gi g

