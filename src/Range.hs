module Range (
  checkOne,
  rangePartition,
) where

import qualified Data.Set as Set
import System.Environment 

import AllocateWords
import Bitable
import LWrappedGraph
import CayleyGraph
import ArcCons
import BitGraph
import ConciseGraph
import DeterminismProperty
import Search
import SmartSearch as SS
import CommonLGraphTypes
import LabeledGraph
import Pretty

checkOne :: Size -> ConciseGraph -> IO ()
checkOne size graph = do
  putStrLn (show graph)
  putStrLn "=============="
  putStr (showem size graph)
  putStrLn (show (searchDbgHomomorphism (conciseGraphI size) 11 graph))
  --putStrLn (show (SS.searchUpTo size 10 graph))
  putStrLn "\n"

easyPathCondition :: Ord x => LabeledGraphI g x -> g -> Bool
easyPathCondition gi g = pathCondition s cayleyGraph where
  bitification = genericBitableI gi g
  s = numBits bitification
  cayleyGraph = rightCayleyGraph bitification

isGood :: Ord x => LabeledGraphI g x -> g -> Bool
isGood gi g = not (isConstructionDeterministic gi g) && easyPathCondition gi g

isCounterexample :: Ord x => LabeledGraphI g x -> g -> Bool
isCounterexample gi graph = let
    dom = LabeledGraph.domain gi graph
    subsets = Set.filter (\s -> Set.size s >= 3) $ Set.powerSet dom
    properSubsets = Set.toList $ Set.filter (\s -> Set.size s < Set.size dom) subsets
    subI = lMapGraphIWithNodePrinter (prettyNode gi graph)
    subgraphs = map (lMapSubgraphFromLGraph gi graph) properSubsets
   in not (isStronglyConstructionDeterministic gi graph)
        && isConstructionDeterministic gi graph
        && all (not . (isGood subI)) subgraphs && easyPathCondition gi graph

rangePartition :: IO ()
rangePartition = do
  let size = 4
  let gi = conciseGraphI size
  let bitmaps = Prelude.filter (notTrivial size) (ConciseGraph.allGraphsOfSize size)
  let lessTrivial = filter (not . hasT1 gi) bitmaps
  let list = Prelude.filter (isCounterexample gi) lessTrivial
  let example = head list
  putStrLn (show (toCode size example))
  putStrLn (showLG (conciseGraphI size) example)
  --putStrLn (show $ length list)

