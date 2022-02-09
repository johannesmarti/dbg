module Range (
  rangeCD,
  findDRange,
  mainRange,
  pathRange,
) where

import LWrappedGraph
import CaleyGraph
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
      then HomoAt level
    else if level >= cutoff then UnknownAt cutoff
    else case liftWithFilter unsoundDominationFilter lifted of
           Nothing -> NoHomo
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
  let filtered = Prelude.filter (\g -> SS.searchUpTo 2 (conciseGraphI 4) g == UnknownAt 2) bitmaps
  let evenMoreFiltered = Prelude.filter (\g -> searchLifting 6 (conciseGraphI 4) g == UnknownAt 6) filtered
  let postFilter = Prelude.map (\g -> (g,SS.searchUpTo 11 (conciseGraphI 4) g)) evenMoreFiltered
  let printer (g,r) = (putStrLn ((show g) ++ ":")) >> (putStrLn (show r))
  mapM_ printer postFilter
  putStrLn (show (Prelude.filter (\p -> snd p == UnknownAt 11) postFilter))

pathRange :: IO ()
pathRange = do
  let size = 4
  let bitmaps = Prelude.filter (notTrivial size) (ConciseGraph.allGraphsOfSize size)
  let cd = Prelude.filter (not . (isConstructionDeterministic (conciseGraphI size))) bitmaps
  --let cd = bitmaps
  let withCg = [(g, caleyGraphOfConcise size g) | g <- cd]
  --let quiteGood = Prelude.filter ((limitedPathCondition size 7) . snd) withCg
  let quiteGood = Prelude.filter ((limitedPathCondition size 3) . snd) withCg
  let list = Prelude.filter (not . weakPathCondition size . snd) quiteGood
  let first = fst $ head list
  print first
  putStrLn (showLG (conciseGraphI size) first)
  --putStrLn (show $ length list)

findDRange :: Int -> IO ()
findDRange n = do
  let factor = 256 * 8 * 4 
  let step = (ConciseGraph.totalGraph 4) `div`  factor
  let start = 12 * 8 * 4 * step
  let end = min (start + step) (ConciseGraph.totalGraph 4)
  let bitmaps = Prelude.filter (notTrivial 4) [start .. end]
  let filtered = Prelude.filter (\g -> SS.searchUpTo n (conciseGraphI 4) g == HomoAt n) bitmaps
  print $ take 4 $ filtered
