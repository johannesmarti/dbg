module Main (
  showem,
  main
) where

import System.Environment

import qualified Data.Set as Set

import LWrappedGraph
import CaleyGraph
import HomoFor41430174
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
main = game
--main = check
--main = mainRange
--main = range3
--main = mapM_ (checkOne 4) unknownAt9
--main = easyLiftingPathReport 3 dbgI (dbg 2)
--main = checkHomo mapGraphI slowSquare
--main = checkHomo mapGraphI slowFour
--main = print $ searchLifting 7 mapGraphI force3d
--main = niceLifting mapGraphI totalIrreflexive

--main = print $ SS.searchUpTo 7 (conciseGraphI 4) 2063974806
--main = print $ SS.searchUpTo 11 (conciseGraphI 4) 23767755
--main = print $ SS.searchUpTo 11 (conciseGraphI 4) 43343050
--main = print $ SS.searchUpTo 12 (conciseGraphI 4) 106801820
--main = checkOne 3 14533
--main = print $ fromLBitGraph 3 $ LWrappedGraph.innerGraph $ fst $ labeledBitify lMapGraphI force3d
--main = print $ SS.searchUpTo 9 (conciseGraphI 4) 23731294
--main = print $ SS.searchUpTo 9 (conciseGraphI 4) 23731294
--main = print $ SS.searchUpTo 9 (conciseGraphI 4) 23731294
--main = easyPathReport (conciseGraphI 4) 1612382568
--main = easyLiftingReport 6 goesWrongI goesWrong
--main = easyLiftingPathReport 4 (conciseGraphI 4) 2063974806
--main = print $ searchLifting 6 (conciseGraphI 4) 2063974806

--main = easyPathReport (conciseGraphI 5) 616005754167427

--main = easyPathReport (conciseGraphI 5) 619373008528515
--main = print $ SS.searchUpTo 13 (conciseGraphI 5) 619373008528515

--main = easyPathReport lMapGraphI slowSquare

--main = easyPathReport lMapGraphI complicatedNePos
--main = print $ SS.searchUpTo 8 lMapGraphI complicatedPos

--main = print $ SS.searchUpTo 11 (conciseGraphI 4) 2685212300
--main = print $ searchLifting 23 (conciseGraphI 4) 4966674
--main = print $ SS.searchUpTo 11 (conciseGraphI 4) 2685212300
--main = easyLiftingPathReport 3 (conciseGraphI 4) 4966674
--main = easyLiftingReport 4 (conciseGraphI 4) 4966674
--main = easyPathReport (conciseGraphI 4) slowFourConcise
--main = easyPathReport (conciseGraphI 4) 4966674
--main = putStrLn $ showLG (conciseGraphI 4) slowFourConcise

--main = easyLiftingPathReport 3 lMapGraphI force3d

--main = print $ SS.searchUpTo 10 lMapGraphI slowSquare
--main = print $ searchLifting 6 lMapGraphI slowSquare

--main = print $ SS.searchUpTo 11 coolSubPatternI coolSubPattern
--main = easyPathReport coolSubPatternI coolSubPattern
--main = easyLiftingReport 4 coolSubPatternI coolSubPattern

--main = print $ SS.searchUpTo 11 (conciseGraphI 4) 3569496551
--main = print $ searchLifting 4 (conciseGraphI 4) 3569496551
--main = easyPathReport coolPatternI coolPattern
--main = easyLiftingReport 4 coolPatternI coolPattern


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

checkOne :: Size -> ConciseGraph -> IO ()
checkOne size graph = do
  putStrLn (show graph)
  putStrLn "=============="
  putStr (showem size graph)
  putStrLn (show (searchDbgHomo (conciseGraphI size) 11 graph))
  --putStrLn (show (SS.searchUpTo size 10 graph))
  putStrLn "\n"

range3 :: IO ()
range3 = do
  let bitmaps = Prelude.filter (notTrivial 3) (ConciseGraph.allGraphsOfSize 3)
  let list = Prelude.filter (\g -> SS.searchUpTo 6 (conciseGraphI 3) g == HomoAt 3) bitmaps
  --let list = Prelude.filter (\g -> searchLifting 6 (conciseGraphI 3) g == HomoAt 3) bitmaps
  putStrLn (show $ length list)

check :: IO ()
check = do
  let size = 4
  let factor = 256 * 4
  let step = (ConciseGraph.totalGraph size) `div`  factor
  let start = 3 * (ConciseGraph.totalGraph 4) `div` 8
  --let start = 0
  let end = min (start + step) (ConciseGraph.totalGraph size)
  --let bitmaps = Prelude.filter (notTrivial 4) [start .. 2063974805]
  let gi = conciseGraphI size
  let pathCond conG = let
          bg = toLBitGraph size conG
          cg = caleyGraphOfLBitGraph size bg
        in isGood size cg
  let bitmaps = Prelude.filter (notTrivial size) [start .. end]
  let weakBm = Prelude.filter (weakPathCondition size . ConciseGraph.toLBitGraph size) bitmaps
  let notDet = Prelude.filter (not . isConstructionDeterministic gi) weakBm
  let hasNoHomo upTo cg = case SS.searchUpTo upTo gi cg of
                            NoHomo   -> True
                            HomoAt _ -> False
                            UnknownAt _ -> True
  let maybeBad = Prelude.filter (hasNoHomo 8) notDet
  let badPairs = Prelude.map (\g -> (g,SS.searchUpTo 11 gi g)) maybeBad
  --putStrLn $ LabeledGraph.showLG gi firstExample
  --easyLiftingPathReport 5 gi firstExample
  let printer (g,r) = (putStrLn ((show g) ++ ":")) >> (putStrLn (show r))
  mapM_ printer badPairs
  putStrLn (show (Prelude.filter (\p -> snd p == UnknownAt 11) badPairs))

mainRange :: IO ()
mainRange = do
  let factor = 256 * 8 * 4 
  let step = (ConciseGraph.totalGraph 4) `div`  factor
  --let start = 5 * (ConciseGraph.totalGraph 4) `div` 8
  let start = 123 * 8 * 4 * step
  let end = min (start + step) (ConciseGraph.totalGraph 4)
  let bitmaps = Prelude.filter (notTrivial 4) [start .. 2063974805]
  --let list = Prelude.filter (\g -> SS.searchUpTo 3 9 g == HomoAt 3) bitmaps
  --let list = Prelude.filter (\g -> SS.searchUpTo (conciseGraphI 3) 6 g == HomoAt 3) bitmaps
  --let list = Prelude.filter (\g -> SS.searchUpTo 4 6 g == HomoAt 6) bitmaps
  --let filtered = Prelude.filter (\g -> SS.searchUpTo 4 (conciseGraphI 4) g == UnknownAt 4) bitmaps
  --let moreFiltered = Prelude.filter (\g -> searchLifting 3 (conciseGraphI 4) g == UnknownAt 3) filtered
  let evenMoreFiltered = Prelude.filter (\g -> SS.searchUpTo 7 (conciseGraphI 4) g == UnknownAt 7) bitmaps
  let postFilter = Prelude.map (\g -> (g,SS.searchUpTo 11 (conciseGraphI 4) g)) evenMoreFiltered
  let printer (g,r) = (putStrLn ((show g) ++ ":")) >> (putStrLn (show r))
  mapM_ printer postFilter
  putStrLn (show (Prelude.filter (\p -> snd p == UnknownAt 11) postFilter))
