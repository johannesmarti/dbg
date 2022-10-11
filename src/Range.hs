module Range (
  rangeCD,
  findDRange,
  findCRange,
  mainRange,
  cdRange,
  forceN,
  pathRange,
  checkOne,
) where

import System.Environment 

import AllocateWords
import LWrappedGraph
import CayleyGraph
import ArcCons
import BitGraph
import ConciseGraph
import DeterminismProperty
import Lifting
import Search
import SmartSearch as SS
import CommonLGraphTypes
import LabeledGraph
import Pretty

searchLifting :: (Pretty x, Ord x) => Int -> LabeledGraphI g x -> g -> Result
searchLifting cutoff gi graph = worker g 0 where
  g = toLiftedGraph gi graph
  worker lifted level =
    if hasT1 liftedGraphI lifted
      then HomomorphismAt level
    else if level >= cutoff then UnknownAt cutoff
    else case liftWithFilter unsoundDominationFilter lifted of
           Nothing -> NoHomomorphism
           Just ll -> worker ll (level + 1)

rangeCD :: IO ()
rangeCD = do
  let size = 3
  let bitmaps = Prelude.filter (notTrivial size) (ConciseGraph.allGraphsOfSize size)
  let lessTrivial = filter (not . hasT1 (conciseGraphI size)) bitmaps
  let cd = Prelude.filter (not . (isConstructionDeterministic (conciseGraphI size))) lessTrivial
  let list = Prelude.filter (not . (SS.subPathCondition (conciseGraphI size))) cd
  print (head list)
  putStrLn (showLG (conciseGraphI size) (head list))
  --putStrLn (show $ length list)

cdRange :: Int -> Integer -> IO ()
cdRange size factor = do
  let b = (ConciseGraph.totalGraph size + 1) `div` factor
  let bitmaps = [0 .. b]
  let cd = Prelude.filter (not . (isConstructionDeterministic (conciseGraphI size))) bitmaps
  putStrLn ((show $ length cd) ++ " of " ++ show b)

forceN :: IO ()
forceN = do
  args <- getArgs
  let n = read (head args)
  let start = 0
  let end = (ConciseGraph.totalGraph 4)
  let bitmaps = Prelude.filter (notTrivial 4) [start .. end]
  let filtered = Prelude.filter (\g -> SS.searchUpTo (n - 1) (conciseGraphI 4) g == UnknownAt (n - 1)) bitmaps
  let mapped = Prelude.map (\g -> (g, SS.searchUpTo n (conciseGraphI 4) g)) filtered
  let moreFiltered = Prelude.filter (\(g,r) -> r == HomomorphismAt n) mapped
  let printer (g,r) = (putStrLn ((show g) ++ ":")) >> (putStrLn (show r))
  mapM_ printer (take 5 moreFiltered)

mainRange :: IO ()
mainRange = do
  --let factor = 256 * 8 * 4 
  let factor = 256
  let step = (ConciseGraph.totalGraph 4) `div`  factor
  --let start = 5 * (ConciseGraph.totalGraph 4) `div` 8
  --let start = 123 * 8 * 4 * step
  let start = 0
  let end = min (start + step) (ConciseGraph.totalGraph 4)
  let bitmaps = Prelude.filter (notTrivial 4) [start .. start + step]
  --let list = Prelude.filter (\g -> SS.searchUpTo 3 9 g == HomomorphismAt 3) bitmaps
  --let list = Prelude.filter (\g -> SS.searchUpTo (conciseGraphI 3) 6 g == HomomorphismAt 3) bitmaps
  --let list = Prelude.filter (\g -> SS.searchUpTo 4 6 g == HomomorphismAt 6) bitmaps
  let filtered = Prelude.filter (\g -> SS.searchUpTo 5 (conciseGraphI 4) g == UnknownAt 5) bitmaps
  --let moreFiltered = Prelude.filter (\g -> searchLifting 3 (conciseGraphI 4) g == UnknownAt 3) filtered
  let mapped = Prelude.map (\g -> (g, SS.searchUpTo 6 (conciseGraphI 4) g)) filtered
  let moreFiltered = Prelude.filter (\(g,r) -> r == HomomorphismAt 6) mapped
  --let moreFiltered = mapped
  --let evenMoreFiltered = Prelude.filter (\g -> searchLifting 6 (conciseGraphI 4) g == UnknownAt 6) filtered
  --let postFilter = Prelude.map (\g -> (g,SS.searchUpTo 11 (conciseGraphI 4) g)) evenMoreFiltered
  let printer (g,r) = (putStrLn ((show g) ++ ":")) >> (putStrLn (show r))
  mapM_ printer (take 10 moreFiltered)
  --putStrLn (show (Prelude.filter (\p -> snd p == UnknownAt 11) postFilter))

pathRange :: IO ()
pathRange = do
  let size = 4
  let bitmaps = Prelude.filter (notTrivial size) (ConciseGraph.allGraphsOfSize size)
  let cd = Prelude.filter (not . (isConstructionDeterministic (conciseGraphI size))) bitmaps
  --let cd = bitmaps
  let withCg = [(g, cayleyGraphOfConcise size g) | g <- cd]
  --let quiteGood = Prelude.filter ((limitedPathCondition size 7) . snd) withCg
  let quiteGood = Prelude.filter ((limitedPathCondition size 3) . snd) withCg
  let list = Prelude.filter (not . weakPathCondition size . snd) quiteGood
  let f10 = map fst $ take 40 $ list
  let first = head f10
  putStrLn (showLG (conciseGraphI size) first)
  print f10
  --putStrLn (show $ length list)

findDRange :: Int -> IO ()
findDRange n = do
  let factor = 256 * 8 * 4 
  let step = (ConciseGraph.totalGraph 4) `div`  factor
  let start = 12 * 8 * 4 * step
  let end = min (start + step) (ConciseGraph.totalGraph 4)
  let bitmaps = Prelude.filter (notTrivial 4) [start .. end]
  let filtered = Prelude.filter (\g -> SS.searchUpTo n (conciseGraphI 4) g == HomomorphismAt n) bitmaps
  print $ take 4 $ filtered

findCRange :: Int -> IO ()
findCRange size = do
  let factor = 1
  let step = (ConciseGraph.totalGraph size) `div`  factor
  --let start = 12 * 8 * 4 * step
  let start = 0
  let impr n = n + 7
  let end = min (start + step) (ConciseGraph.totalGraph size)
  let bitmaps = Prelude.filter (notTrivial size) [start .. end]
  let cd = Prelude.filter (not . (isConstructionDeterministic (conciseGraphI size))) bitmaps
  let withCg = [(g, cayleyGraphOfConcise size g) | g <- cd]
  let withPathCondition = Prelude.filter ((pathCondition size) . snd) withCg
  let withAllocationLevel = [(g,cg,firstLevelToAllocate size (relationOfWord size cg)) | (g, cg) <- withPathCondition]
  let withBoth = Prelude.map (\(g,cg,n) -> (g,cg,n,SS.searchUpTo (impr n) (conciseGraphI size) g)) withAllocationLevel
  let towering = Prelude.filter (\(g,cg,n,r) -> r == UnknownAt (impr n)) withBoth
  let prettier = Prelude.map (\(g,cg,n,r) -> (g,n,r)) towering
  --let prettier = Prelude.map (\(g,cg,n,r) -> (g,n,r)) withBoth
  print $ take 3 $ prettier

checkOne :: Size -> ConciseGraph -> IO ()
checkOne size graph = do
  putStrLn (show graph)
  putStrLn "=============="
  putStr (showem size graph)
  putStrLn (show (searchDbgHomomorphism (conciseGraphI size) 11 graph))
  --putStrLn (show (SS.searchUpTo size 10 graph))
  putStrLn "\n"

