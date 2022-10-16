module Main (
  main
) where

--import System.Environment 

import Patterns
import LabeledGraph
import Report

wordList = [[One,One,One,One,One,Zero],[Zero,Zero,One,One,One,One],[Zero,One,Zero,One,One,One],[Zero,One,One,Zero,One,One],[Zero,One,One,One,Zero,One]]
(gi, g) = (big5I,big5)
--(gi, g) = (force3dI,force3d)
--(gi, g) = (b1ef5I,b1ef5)
--(gi, g) = (specialUnfoldI,specialUnfold)
--(gi, g) = (biggestI,biggest)
main = do
  putStr . unlines $ prettyLabeledGraph gi g
  putChar '\n'
  putStr . unlines $ spiralReport wordList gi g
