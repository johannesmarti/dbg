module Game (
  easyGame,
  nd,
  newReflIn,
  remove,
  noop,
) where

import qualified Data.Set as Set
import Data.List (maximumBy,intercalate)

import Reports
import LabeledGraph
import CommonLGraphTypes
import Graph
import BitGraph
import Lifting

import Tools

nd :: (x -> Bool) -> (x -> Bool) -> x -> Bool
nd f g a = f a && g a

newReflIn :: Size -> BitGraph -> Lifted Node -> Bool
newReflIn size bg v = case v of
  Doubleton _ _ -> liftedRelation (\x y -> Graph.hasArc (bitGraphI size) bg (x,y)) v v
  otherwise     -> True

remove :: Eq x => [Lifted x] -> Lifted x -> Bool
remove remList v = not (v `elem` remList)

noop :: Lifted x -> Bool
noop x = True

gameReport :: Ord x => Int -> Int -> LabeledGraphI g x -> g -> [Lifted x -> Bool] -> [String]
gameReport numWords bound gi graph filterList =
  let
      lI = liftedGraphIWithNodePrinter (LabeledGraph.prettyNode gi graph)
      baseLiftedGraph = toLiftedGraph gi graph
      lifts = take bound $ takeTill (hasDoubleRefl lI) $ map fst $ untilNothing iterator (baseLiftedGraph,filterList)
      lifter = liftWithFilter weakDominationFilter
      iterator (g,(hf:fs)) = case lifter g of
        Nothing -> Nothing
        Just lifted -> let flifted = lMapSubgraphFromLGraph lI lifted dom
                           oldDom = LabeledGraph.domain lI lifted
                           dom = Set.filter hf oldDom
                         in Just (flifted,fs)
      lReport iface gra = wordReport numWords iface gra
      printer g = lReport lI g
      graphToSize g = Set.size $ LabeledGraph.domain lI g
  in lReport gi graph ++
     ["=============="] ++
     ["Size of the liftings: " ++ show (map graphToSize lifts)] ++
     intercalate ["", "+++++++++++++", ""] (map printer lifts)

easyGame :: Ord x => Int -> Int -> LabeledGraphI g x -> g -> [Lifted x -> Bool] -> IO ()
easyGame numWords b gi g filterList = putStr . unlines $ (gameReport numWords b gi g (filterList ++ cycle [\_ -> True]))
