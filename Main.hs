module Main (
  main
) where

import System.Environment

import qualified Data.Set as Set

import LWrappedGraph
import CaleyGraph
import Report
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
import Range
import Search
import SmartSearch as SS
import Bitify
import CommonLGraphTypes
import Patterns
import Pretty
import LabeledGraph

main :: IO ()
--main = cdRange 4 (1024 * 4)
--main = cdRange 3 1
--main = pathRange

--main = print $ toConcise dbgI (dbg 2)

--main = easyPathReport zoCompI zoComp
--main = print $ deterministicAntichain zoCompI zoComp

--main = easyWordReport 7 ex3I ex3
--main = easyLiftingPathReport 5 ex3I ex3
main = easyLiftingReport 5 ex4I ex4
--main = easyPathReport ex4I ex4
--main = easyLiftingReport 5 zoCompI zoComp

--main = easyWordReport 31 slowSquareI slowSquare

--main = easyLiftingReport 5 force3dI force3d

--main = easyLiftingReport 5 slowSquareI slowSquare
--main = easyWordReport 15 (conciseGraphI 4) 3937920

--main = game

--main = easyLiftingPathReport 3 lMapGraphI slowSquare
--main = easyLiftingPathReport 2 (conciseGraphI caleySchreckSize) caleySchreck
--main = easyLiftingPathReport 2 lMapGraphI force3d
--main = check
--main = mainRange
--main = range3
--main = mapM_ (checkOne 4) unknownAt9
--main = easyLiftingPathReport 3 dbgI (dbg 2)
--main = checkHomo mapGraphI slowSquare
--main = checkHomo mapGraphI slowFour
--main = print $ searchLifting 7 (conciseGraphI 4) 2063925436
--main = easyLiftingReport 7 (conciseGraphI 4) 2063925436
--main = easyLiftingPathReport 5 unsoundI unsound
--main = easyLiftingPathReport 5 (conciseGraphI 4) 2063974806
--main = print $ SS.searchUpTo 7 (conciseGraphI 4) 2063931814
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

checkOne :: Size -> ConciseGraph -> IO ()
checkOne size graph = do
  putStrLn (show graph)
  putStrLn "=============="
  putStr (showem size graph)
  putStrLn (show (searchDbgHomo (conciseGraphI size) 11 graph))
  --putStrLn (show (SS.searchUpTo size 10 graph))
  putStrLn "\n"

