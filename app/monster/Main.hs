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

--(gi, g) = (alloc2I,alloc2)
(gi, g) = (big5I,big5)
--(gi, g) = (force3dI,force3d)
--(gi, g) = (b1ef5I,b1ef5)
--(gi, g) = (specialUnfoldI,specialUnfold)
--(gi, g) = (biggestI,biggest)
pgi = powerGraphI gi
main = do
  putStr . unlines $ prettyLabeledGraph gi g
  putChar '\n'
  print $ SS.searchUpTo 10 gi g
  putChar '\n'
  putStr . unlines $ prettyBigLabeledGraph (converseI pgi) g 
  putChar '\n'
  putStr . unlines . (prettyReachability (prettyNode pgi g)) $ universalReachability pgi g (Set.map Set.singleton (domain gi g)) 
  putChar '\n'
  easySpiralReport 8 gi g

