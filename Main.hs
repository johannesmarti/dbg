module Main (
  showem,
  main
) where

import System.Environment

import Data.Set as Set

import ArcCons
import ConciseGraph
import DeBruijn
import DeterminismProperty
import Graph
import Homo
import MapGraph
import Search
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

showem :: Size -> ConciseGraph -> String
showem size graph = unlines $ prettyGraph (conciseGraphI size) show graph

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  putStrLn (showem 3 14731)
  putStrLn (show (searchDbgHomo (conciseGraphI 3) 6 14731))
  putStrLn (show (SS.searchUpTo 3 6 14731))
  --let bitmaps = filter (notTrivial 4) (allGraphsOfSize 4)
  --return ()
  --let bitmaps = filter (notTrivial 4) [start .. start + step]
  --let bitmaps = Prelude.filter (notTrivial 3) (allGraphsOfSize 3)
  --let list = filter ((CS.homoLargerThan 3 6 2)) bitmaps
  --let list = Prelude.filter ((SS.homoLargerThan 4 6 4)) bitmaps
  --let list = Prelude.filter ((CS.homoLargerThan 3 6 2)) bitmaps
  --let list = Prelude.filter (SS.noResultUpTo 3 3) bitmaps
  --let list = filter ((CS.homoLargerThan 4 6 4)) bitmaps
  --putStrLn (show $ length list)
  --putStrLn (show $ head list)
