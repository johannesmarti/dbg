module Main (
  main
) where

--import System.Environment 

import Patterns
import ConstructionGraph
import LabeledGraph (showLG)

main :: IO ()
main = do
  let (gi, g) = (force3dI,force3d)
  putStrLn $ showLG (constructionGraphI gi) g
  print $ immediatelyConstructible gi g
