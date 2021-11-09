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

main :: IO ()
--main = mapM_ (checkOne 4) unknownAt9
main = mainRange

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

checkOne :: Size -> ConciseGraph -> IO ()
checkOne size graph = do
  putStrLn (show graph)
  putStrLn "=============="
  putStr (showem size graph)
  --putStrLn (show (searchDbgHomo (conciseGraphI size) 10 graph))
  putStrLn (show (SS.searchUpTo size 10 graph))
  putStrLn "\n"

unknownAt9 :: [ConciseGraph]
unknownAt9 = [4003476,4019856,4019860,4039000,4041040,4041048,4065821,4065885,4065949,4066005,4066013]

mainRange :: IO ()
mainRange = do
  --args <- getArgs
  --let n = read (head args) :: Int
  let start = 3938472
  let step = (totalGraph 4) `div` (1024 * 32)
  let bitmaps = Prelude.filter (notTrivial 4) [start .. start + step]
--  let bitmaps = Prelude.filter (notTrivial 3) (allGraphsOfSize 3)
  ----let bitmaps = Prelude.filter (notTrivial 4) (allGraphsOfSize 4)
  --let list = Prelude.filter (\g -> SS.searchUpTo 4 9 g == HomoAt 9) bitmaps
  --let list = Prelude.filter (\g -> SS.searchUpTo 3 9 g == HomoAt 3) bitmaps
  --let list = Prelude.filter (\g -> SS.searchUpTo 3 2 g == Unknown 2) bitmaps
  --let list = Prelude.filter (\g -> SS.searchUpTo 4 10 g == HomoAt 10) bitmaps
  let list = Prelude.filter (\g -> SS.searchUpTo 4 9 g == UnknownAt 9) bitmaps
  putStrLn (show $ length list)
  --mapM_ (checkOne 3) list
  --putStrLn (show $ list)
