module Programs.Range (
  checkOne,
  rangePartition,
  otherCounterexample,
) where

import qualified Data.Set as Set

import Bitify.Bitifier
import Conditions.CayleyGraph
import Graphs.BitGraph
import Graphs.ConciseGraph
import Conditions.Constructible
import HomomorphismSearch.Search
import Graphs.CommonLabeledGraphTypes
import Graphs.LabeledGraphInterface as LGI

checkOne :: Size -> ConciseGraph -> IO ()
checkOne size graph = do
  putStrLn (show graph)
  putStrLn "=============="
  putStr (showem size graph)
  putStrLn (show (searchDbgHomomorphism (conciseGraphInterface size) 11 graph))
  --putStrLn (show (SS.searchUpTo size 10 graph))
  putStrLn "\n"

easyPathCondition :: Ord x => LabeledGraphInterface g x -> g -> Bool
easyPathCondition gi g = pathCondition s cayleyGraph where
  bitification = genericBitifier gi g
  s = numBits bitification
  cayleyGraph = rightCayleyGraph bitification

isGood :: Ord x => LabeledGraphInterface g x -> g -> Bool
isGood gi g = isConstructible gi g && easyPathCondition gi g

isCounterexample :: Ord x => LabeledGraphInterface g x -> g -> Bool
isCounterexample gi graph = let
    dom = LGI.domain gi graph
    subsets = Set.filter (\s -> Set.size s >= 3) $ Set.powerSet dom
    properSubsets = Set.toList $ Set.filter (\s -> Set.size s < Set.size dom) subsets
    subI = labeledMapGraphInterfaceWithNodePrinter (prettyNode gi graph)
    subgraphs = map (labeledMapSubgraphFromLabeledGraph gi graph) properSubsets
   in isStronglyConstructible gi graph
        && not (isConstructible gi graph)
        && all (not . (isGood subI)) subgraphs && easyPathCondition gi graph

rangePartition :: IO ()
rangePartition = do
  let size = 4
  let gi = conciseGraphInterface size
  let bitmaps = Prelude.filter (notTrivial size) (Graphs.ConciseGraph.allLabeledGraphsOfSize size)
  let lessTrivial = filter (not . hasT1 gi) bitmaps
  let list = Prelude.filter (isCounterexample gi) lessTrivial
  let example = head list
  putStrLn (show (toCode size example))
  putStrLn (showLG (conciseGraphInterface size) example)
  --putStrLn (show $ length list)

otherCounterexample :: IO ()
otherCounterexample = do
  let size = 3
  let gi = conciseGraphInterface size
  let bitmaps = Prelude.filter (missesLoop size) (Graphs.ConciseGraph.allLabeledGraphsOfSize size)
  let list = filter (isConstructible gi) bitmaps
  let example = head list
  print (toCode size example)
  putStrLn (showLG gi example)
  --putStrLn (show $ length list)

