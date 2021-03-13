module Main (
  main
) where

import System.Environment

import ArcCons
import BitGraph
import DeBruijn
import Homo
import Search

import Patterns

dg = deBruijnGraph

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  --putStrLn (show (arcConsHomos (dg n) (testPattern)))
  --putStrLn (show (searchForDbgHomo n (allPaths)))
  let list = filter ((homoLargerThan 3 2) . (bitGraph 3)) (allGraphsOfSize 3)
  putStrLn (show (map (bitGraph 3) list))
  --putStrLn (show $ length list)
