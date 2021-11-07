module Main (
  showem,
  main
) where

import System.Environment

import qualified Data.Set as Set

import ArcCons
import ConciseGraph
import DeBruijn
import DeterminismProperty
import Graph
import Homo
import MapGraph
import Search
import SmartSearch as SS

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
  --args <- getArgs
  --let n = read (head args) :: Int
  putStrLn (showem 4 3942849)
  putStrLn (show (searchDbgHomo (conciseGraphI 4) 8 3942849))
  putStrLn (show (SS.searchUpTo 4 9 3942849))
  --let bitmaps = filter (notTrivial 4) (allGraphsOfSize 4)
  --return ()
  --let start = 3938472
  --let step = (totalGraph 4) `div` (1024 * 512)
  --let bitmaps = Prelude.filter (notTrivial 4) [start .. start + step]
  ----let bitmaps = Prelude.filter (notTrivial 3) (allGraphsOfSize 3)
  ----let bitmaps = Prelude.filter (notTrivial 4) (allGraphsOfSize 4)
  --let list = Prelude.filter (\g -> SS.searchUpTo 4 8 g == Unknown) bitmaps
  ----let list = filter ((CS.homoLargerThan 4 6 4)) bitmaps
  --putStrLn (show $ head list)
  ----putStrLn (show $ head list)
