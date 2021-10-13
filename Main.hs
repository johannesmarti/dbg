module Main (
  main
) where

import System.Environment

import ArcCons
import BitGraph
import DeBruijn
import Homo
import Search
import qualified CaleySearch as CS
import qualified SmartSearch as SS

import Patterns

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  --putStrLn (show (arcConsHomos (dbg n) (testPattern)))
  --putStrLn (show (searchDbgHomo n (allPaths)))
  --let bitmaps = filter (notTrivial 4) (allGraphsOfSize 4)
  let total = totalGraph 4
  --let q = 1024 * 8
  let q = 1024 * 512
  let p = (345 + 20)
  let step = total `div` q
  let start = 561888294
  --let bitmaps = filter (notTrivial 4) [start .. start + step]
  let bitmaps = filter (notTrivial 3) (allGraphsOfSize 3)
  --let list = filter ((CS.homoLargerThan 3 6 2)) bitmaps
  let list = filter ((SS.homoLargerThan 3 6 2)) bitmaps
  --let list = filter ((CS.homoLargerThan 4 6 4)) bitmaps
  --putStrLn (show (map (bitGraph 3) list))
  putStrLn (show $ length list)
