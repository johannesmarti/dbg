module Programs.SpiralReport (
  Programs.SpiralReport.spiralReport
) where

import System.Environment 
import Data.Set as Set

import Programs.Report
import Examples.Patterns
import Data.Label
import Spiral
import Conditions.ConstructionGraph
import Graphs.DeBruijnGraph
import Graphs.LabeledGraphInterface

import qualified HomomorphismSearch.SmartSearch as SS


spiralReport :: IO ()
--(gi, g) = (slowLiftingI, slowLifting)
--(gi, g) = (dbgI,dbg 4)
--(gi, g) = (alloc3I,alloc3)
--(gi, g) = (big5I,big5)
--(gi, g) = (force9dI,force9d)
(gi, g) = (force6dInterface,force6d)
--(gi, g) = (b1ef5I,b1ef5)
--(gi, g) = (specialUnfoldI,specialUnfold)
--(gi, g) = (biggestI,biggest)
--(gi, g) = (hamburgerI,hamburger)
pgi = powerGraphInterface gi
spiralReport = do
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

