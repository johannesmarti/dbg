module Main (
  main
) where

--import System.Environment 

import Examples.Patterns
import Graphs.LabeledGraphInterface
import Report
import Data.Label

wordList = [[Zero,Zero,Zero,Zero,One]
         ] 
--wordList = [[Zero,Zero,One,One,Zero,One]]
--(gi, g) = (big5I,big5)
--(gi, g) = (alloc3I,alloc3)
(gi, g) = (force6dInterface,force6d)
--(gi, g) = (force9dI,force9d)
--(gi, g) = (b1ef5I,b1ef5)
--(gi, g) = (specialUnfoldI,specialUnfold)
--(gi, g) = (biggestI,biggest)
main :: IO ()
main = do
  --putStr . unlines $ prettyLabeledGraph gi g
  putStrLn "++++++++++++++++++++++"
  putChar '\n'
  putStr . unlines $ spiralReport wordList gi g
