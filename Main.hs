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
--main = niceLifting (conciseGraphI slowFourSize) slowFourConcise
--main = niceLifting dbgI (dbg 1)
--main = niceLifting mapGraphI celtic
--main = easyReport lMapGraphI force3d
--main = easyPathReport dbgI (dbg 2)
--main = niceLifting mapGraphI slowSquare
--main = easyReport mapGraphI slowFour
--main = easyReport (conciseGraphI 4) 3937948
--main = easyReport (conciseGraphI 4) 4040284
--main = easyReport (conciseGraphI 4) 65967450
--main = easyReport dbgI (dbg 3)
--main = checkHomo mapGraphI slowSquare
--main = checkHomo mapGraphI slowFour
--main = print $ searchLifting 7 mapGraphI force3d
--main = niceLifting mapGraphI totalIrreflexive
--main = mainRange
--main = checkOne 4 3937948
--main = niceLifting (conciseGraphI diverger3Size) diverger3
--main = niceLifting lMapGraphI force3d
main = easyLiftingReport 4 dbgI (dbg 2)
--main = niceLifting (conciseGraphI 4) 3946697
--main = checkHomo (conciseGraphI 4) 3946697
--main = niceLifting (conciseGraphI 4) 3941826
--main = checkHomo (conciseGraphI 4) 3941826


{-
searchLifting :: Ord x => Int -> GraphI g x -> g -> Result
searchLifting cutoff gi graph = worker g 0 where
  g = toLiftedGraph gi graph
  worker lifted level =
    if hasDoubleRefl liftedGraphI lifted
      then HomoAt level
    else if level >= cutoff then UnknownAt cutoff
    else case lift lifted of
           Nothing -> NoHomo
           Just ll -> worker ll (level + 1)

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
  let start = 8273023 
  let step = (ConciseGraph.totalGraph 4) `div` (1024 * 32 * 32)
  --let bitmaps = Prelude.filter (notTrivial 4) [start .. start + step]
  let bitmaps = Prelude.filter (notTrivial 3) (ConciseGraph.allGraphsOfSize 3)
  --let list = Prelude.filter (\g -> SS.searchUpTo 3 9 g == HomoAt 3) bitmaps
  let list = Prelude.filter (\g -> SS.searchUpTo (conciseGraphI 3) 6 g == HomoAt 3) bitmaps
  --let list = Prelude.filter (\g -> SS.searchUpTo 4 6 g == HomoAt 6) bitmaps
  --let bad = Prelude.filter (\g -> searchLifting 9 (conciseGraphI 3) g == NoHomo) list
  --let bad = Prelude.filter (\g -> searchLifting 6 (conciseGraphI 4) g /= HomoAt 6) list
  putStrLn (show $ length $ list)
  --putStrLn (show $ length bad)
  --putStrLn (show $ head $ bad)
  --mapM_ (checkOne 3) list
