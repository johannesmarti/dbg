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

import Patterns

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  --putStrLn (show (arcConsHomos (dbg n) (testPattern)))
  --putStrLn (show (searchDbgHomo n (allPaths)))
  let bitmaps = filter (notTrivial 4) (allGraphsOfSize 4)
  --let list = filter ((homoLargerThan (bitGraphI 3) 6 2)) bitmaps
  let list = filter ((CS.homoLargerThan 4 5 3)) bitmaps
  --putStrLn (show (map (bitGraph 3) list))
  putStrLn (show $ length list)
