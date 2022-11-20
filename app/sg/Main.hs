module Main (
  main
) where

--import System.Environment 

import Patterns
import LabeledGraph
import Report

--wordList = [[One,One,One,One,Zero]]
wordList = [[Zero,Zero,Zero,Zero,One]]
(gi, g) = (big5I,big5)
--(gi, g) = (alloc3I,alloc3)
--(gi, g) = (force3dI,force3d)
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
