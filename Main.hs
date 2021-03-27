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

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  --putStrLn (show (arcConsHomos (dbg n) (testPattern)))
  --putStrLn (show (searchDbgHomo n (allPaths)))
  let bitmaps = filter (notTrivial 3) (allGraphsOfSize 3)
  let list = filter ((homoLargerThan (bitGraphI 3) 3 2)) bitmaps
  --putStrLn (show (map (bitGraph 3) list))
  putStrLn (show $ length list)
