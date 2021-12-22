module Game (
  easyGame,
  nd,
  newReflIn,
  remove,
) where

import qualified Data.Set as Set
import Data.List (maximumBy,intercalate)

import ConciseGraph
import FctGraph
import Bitify
import LWrappedGraph
import qualified WrappedGraph as WG
import LabeledGraph
import CommonLGraphTypes
import CaleyGraph
import BitGraph
import Coding
import Pretty
import Graph as Graph
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

gameReport :: Ord x => Int -> LabeledGraphI g x -> g -> [Lifted x -> Bool] -> [String]
gameReport bound gi graph filterList =
  let
      lI = liftedGraphIWithNodePrinter (LabeledGraph.prettyNode gi graph)
      baseLiftedGraph = toLiftedGraph gi graph
      lifts = take bound $ takeTill (hasDoubleRefl lI) $ map fst $ untilNothing iterator (baseLiftedGraph,filterList)
      lifter = liftWithFilter dominationFilter
      iterator (g,(hf:fs)) = case lifter g of
        Nothing -> Nothing
        Just lifted -> let flifted = lMapSubgraphFromLGraph lI lifted dom
                           oldDom = LabeledGraph.domain lI lifted
                           dom = Set.filter hf oldDom
                         in Just (flifted,fs)
      fctGraph l bg = let wg = WG.WrappedGraph bg c undefined
                      in fctGraphFromDomain (LabeledGraph.domain liftedGraphINotPretty l) (liftedRelation (\x y -> Graph.hasArc (WG.wrappedGraphI (bitGraphI s)) wg (x,y)))
      printSuper l r = Graph.prettyGraph (fctGraphIWithNodePrinter (LabeledGraph.prettyNode lI l)) (fctGraph l r)
      printSuperRel l r = (printNodeWithSuccs cg r ++ ":") : (printSuper l r)
      cgRels = CaleyGraph.domain cg
      printLifting lg = intercalate [""] $ (map (printSuperRel lg) (Set.toList wfs) ++ map (printSuperRel lg) (Set.toList nwfs))
      graphToSize g = Set.size $ LabeledGraph.domain lI g
      (wg,s) = labeledBitify gi graph
      inner = innerGraph wg
      cg = caleyGraphOfLBitGraph s inner
      c = coding wg
      enc = encode c
      dec = decode c
      printRel r = Graph.prettyGraph (WG.wrappedGraphI (bitGraphI s)) (WG.WrappedGraph r c (LabeledGraph.prettyNode gi graph))
      printRelWithCode r = (printNodeWithSuccs cg r ++ ":") : (printRel r)
      wfs = wellfoundedElements cg
      nwfs = nonWellfoundedElements cg
      finWords = finiteWords s cg
      (longestFinWord,relOfLongest) = maximumBy (\(a,_) (b,_) -> compare (length a) (length b)) finWords
  in  prettyLabeledGraph gi graph ++
      ["=============="] ++
      ["The Caley graph has " ++ show (Set.size wfs) ++ " finite and " ++
                    show (Set.size nwfs) ++ " infinite elements.", "",
       "It " ++ (if isGood s cg then "satisfies" else "does not satisfy") ++ " the path condition.", "",
       "It's finite words are:", show (map fst finWords),
       "Of which one with maximal length is " ++ show longestFinWord ++ ".", ""] ++
      ["The full CaleyGraph is:"] ++ prettyCaleyGraph cg ++
      ["", "The complete list of its finite elements is:"] ++
       concatMap printRelWithCode (Set.toList wfs) ++
      ["", "The complete list of its infinite elements is:"] ++
      concatMap printRelWithCode (Set.toList nwfs) ++
      ["=============="] ++
      ["Size of the liftings: " ++ show (map graphToSize lifts)] ++ [""] ++
      intercalate ["", "==========", ""] (map printLifting lifts)

easyGame :: Ord x => Int -> LabeledGraphI g x -> g -> [Lifted x -> Bool] -> IO ()
easyGame b gi g filterList = putStr . unlines $ (gameReport b gi g (filterList ++ cycle [\_ -> True]))
