module Main (
  main
) where

import LabeledGraph
import Patterns


main :: IO ()
main = do
  putStrLn (showLG biggestI biggest)
