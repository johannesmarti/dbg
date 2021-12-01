module Main (
  showem,
  main
) where

import System.Environment

import qualified Data.Set as Set

import Reports
import AssocGraph
import ArcCons
import BitGraph
import ConciseGraph
import DeBruijnGraph
import DeterminismProperty
import qualified Graph
import Homo
import Lifting
import MapGraph
import WrappedGraph
import Search
import SmartSearch as SS
import Bitify
import CommonLGraphTypes
import Patterns
import Pretty
import LabeledGraph

main :: IO ()
--main = mapM_ (checkOne 4) unknownAt9
--main = easyPathReport dbgI (dbg 2)
--main = checkHomo mapGraphI slowSquare
--main = checkHomo mapGraphI slowFour
--main = print $ searchLifting 7 mapGraphI force3d
--main = niceLifting mapGraphI totalIrreflexive
--main = mainRange
--main = print $ searchLifting 7 (conciseGraphI 4) 4072605
--main = print $ SS.searchUpTo 11 (conciseGraphI 4) 4072605
--main = easyLiftingReport 4 (conciseGraphI 4) 4072605
--main = easyPathReport (conciseGraphI 4) 4072605

--main = print $ SS.searchUpTo 11 coolSubPatternI coolSubPattern
--main = easyPathReport coolSubPatternI coolSubPattern
--main = easyLiftingReport 4 coolSubPatternI coolSubPattern

main = easyLiftingReport 4 coolPatternI coolPattern


--main = print $ searchLifting 7 (conciseGraphI 4) 8281106
--main = print $ SS.searchUpTo 11 (conciseGraphI 4) 8281106
--main = easyLiftingReport 5 (conciseGraphI 4) 8281106
--main = easyLiftingPathReport 4 lMapGraphI force3d
--main = print $ searchLifting 4 lMapGraphI force3d
--main = easyLiftingPathReport 4 dbgI (dbg 3)
--main = checkHomo (conciseGraphI 4) 3946697
--main = checkHomo (conciseGraphI 4) 3941826


searchLifting :: (Pretty x, Ord x) => Int -> LabeledGraphI g x -> g -> Result
searchLifting cutoff gi graph = worker g 0 where
  g = toLiftedGraph gi graph
  worker lifted level =
    if hasDoubleRefl liftedGraphI lifted
      then HomoAt level
    else if level >= cutoff then UnknownAt cutoff
    else case liftWithFilter dominationFilter lifted of
           Nothing -> NoHomo
           Just ll -> worker ll (level + 1)

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

{-
checkHomo :: (Show x, Ord x) => GraphI g x -> g -> IO ()
checkHomo gi graph = let
    wg = bitify gi graph
    (cg,size) = BitGraph.toConciseGraph (innerGraph wg)
  in do putStrLn . unlines $ prettyGraph gi show graph
        checkOne size cg
-}

{-
checkOne :: Size -> ConciseGraph -> IO ()
checkOne size graph = do
  putStrLn (show graph)
  putStrLn "=============="
  putStr (showem size graph)
  --putStrLn (show (searchDbgHomo (conciseGraphI size) 10 graph))
  putStrLn (show (SS.searchUpTo size 10 graph))
  putStrLn "\n"
-}

mainRange :: IO ()
mainRange = do
  --args <- getArgs
  --let n = read (head args) :: Int
  let start = 4003000
  let step = (ConciseGraph.totalGraph 4) `div` (1024 ) 
  let bitmaps = Prelude.filter (notTrivial 4) [start .. start + step]
  --let bitmaps = Prelude.filter (notTrivial 3) (ConciseGraph.allGraphsOfSize 3)
  --let list = Prelude.filter (\g -> SS.searchUpTo 3 9 g == HomoAt 3) bitmaps
  --let list = Prelude.filter (\g -> SS.searchUpTo (conciseGraphI 3) 6 g == HomoAt 3) bitmaps
  --let list = Prelude.filter (\g -> SS.searchUpTo 4 6 g == HomoAt 6) bitmaps
  let filtered = Prelude.filter (\g -> SS.searchUpTo 9 (conciseGraphI 4) g == UnknownAt 9) bitmaps
  let hehe = Prelude.filter (\g -> searchLifting 4 (conciseGraphI 4) g == UnknownAt 4) filtered
  --let list = Prelude.filter (\g -> searchLifting 6 (conciseGraphI 3) g == UnknownAt 3) bitmaps
  --let bad = Prelude.filter (\g -> searchLifting 9 (conciseGraphI 3) g == NoHomo) list
  --let bad = Prelude.filter (\g -> searchLifting 6 (conciseGraphI 4) g /= HomoAt 6) list
  --let hehe = Prelude.filter (\g -> SS.searchUpTo (conciseGraphI 4) 8 g == UnknownAt 8) list
  --putStrLn (show $ length $ list)
  putStrLn (show $ take 1 $ filtered)
  putStrLn (show $ take 1 $ hehe)
  --putStrLn (show $ length bad)
  --putStrLn (show $ head $ bad)
  --mapM_ (checkOne 3) list
