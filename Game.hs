module Game (
  game,
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

b0 = bn 0
b1 = bn 1
b2 = bn 2
b3 = bn 3
d03 = du b0 b3
d12 = du b1 b2
dd12 = deepen d12
ddd12 = deepen dd12
dddd12 = deepen ddd12
sss0 = si . si . si $ b0
sss1 = si . si . si $ b1
sss2 = si . si . si $ b2
sss3 = si . si . si $ b3
ssb03 = si . si $ d03
dsss01 = du sss0 sss1
dsss02 = du sss0 sss2
dsss2ssb03 = du sss2 ssb03
s0 = si b0
s1 = si b1
s2 = si b2
s3 = si b3
ds13 = du s1 s3
sds13 = si ds13
dsss2sds13 = du sss2 sds13
ss0 = si s0
ss2 = si s2
ss3 = si s3
dss02 = du ss0 ss2
dsss3dss02 = du sss3 dss02
ssd03 = si . si $ d03
dssd03dss02 = du ssd03 dss02
list4 = [dddd12,dsss01,dsss02,dsss2ssb03,dsss2sds13,dsss3dss02,dssd03dss02]
ssss2 = si . si $ ss2
ssss3 = si . si $ ss3
dssss23 = du ssss2 ssss3
list5 = [dssss23] ++ map deepen list4
list6 = map deepen list5
list7 = map deepen list6
list8 = map deepen list7
list9 = map deepen list8
game = easyGame 9 (conciseGraphI 4) 2063974806 [remove [d12],
  remove [dd12],
  remove [ddd12],
  remove list4,
  remove list5 `nd` newReflIn 4 49558,
  remove list6 `nd` newReflIn 4 49558,
  remove list7 `nd` newReflIn 4 49558,
  remove list8 `nd` newReflIn 4 49558,
  remove list9 `nd` newReflIn 4 49558 ]

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
