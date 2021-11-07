module Main (
  main
) where

import System.Environment

import Data.Set as Set

import ArcCons
import BitGraph
import DeBruijn
import DeterminismProperty
import Graph
import Homo
import MapGraph
import Search
import qualified CaleySearch as CS
import qualified SmartSearch as SS

import Patterns

{-
main :: IO ()
main = let res = arcConsHomos dbgI mapGraphI (dbg 8) strange3
           allNodes = domain mapGraphI strange3
           subsets = Set.powerSet allNodes
           (detSubsets,ndetSubsets) = partition (hasDeterminismProperty mapGraphI strange3) subsets
 in do
      putStrLn ("homos from (dbg 8): " ++ show res)
      putStrLn ("subsets: " ++ show subsets)
      putStrLn ("detSubsets: " ++ show detSubsets)
      putStrLn ("ndetSubsets: " ++ show ndetSubsets)
-}

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  --putStrLn (show (SS.homoLargerThan 3 8 2 15274))
  --putStrLn (show (searchDbgHomo n (allPaths)))
  --let bitmaps = filter (notTrivial 4) (allGraphsOfSize 4)
  --return ()
  --let bitmaps = filter (notTrivial 4) [start .. start + step]
  let bitmaps = Prelude.filter (notTrivial 4) (allGraphsOfSize 4)
  --let list = filter ((CS.homoLargerThan 3 6 2)) bitmaps
  let list = Prelude.filter ((SS.homoLargerThan 4 6 4)) bitmaps
  --let list = Prelude.filter ((CS.homoLargerThan 3 6 2)) bitmaps
  --let list = filter ((CS.homoLargerThan 4 6 4)) bitmaps
  putStrLn (show $ length list)
