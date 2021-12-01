module Reports (
  pathReport, easyPathReport,
  liftingReport, easyLiftingReport,
  liftingPathReport, easyLiftingPathReport,
) where

import qualified Data.Set as Set
import Data.List (maximumBy,intercalate)

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

pathReport :: Ord x => LabeledGraphI g x -> g -> [String]
pathReport gi g = let
    (wg,s) = labeledBitify gi g
    inner = innerGraph wg
    cg = caleyGraphOfLBitGraph s inner
    c = coding wg
    enc = encode c
    dec = decode c
    printRel r = Graph.prettyGraph (WG.wrappedGraphI (bitGraphI s)) (WG.WrappedGraph r c (LabeledGraph.prettyNode gi g))
    printRelWithCode r = (printNodeWithSuccs cg r ++ ":") : (printRel r)
    wfs = wellfoundedElements cg
    nwfs = nonWellfoundedElements cg
    finWords = finiteWords s cg
    (longestFinWord,relOfLongest) = maximumBy (\(a,_) (b,_) -> compare (length a) (length b)) finWords
  in ["About the Caley-graph of the pattern:"] ++
      LabeledGraph.prettyLabeledGraph gi g ++
     ["It has " ++ show (Set.size wfs) ++ " finite and " ++
                   show (Set.size nwfs) ++ " infinite elements.", "",
      "It " ++ (if isGood s cg then "satisfies" else "does not satisfy") ++ " the path condition.", "",
      "It's finite words are:", show (map fst finWords),
      "Of which one with maximal length is " ++ show longestFinWord ++ ".", ""] ++
     ["The full CaleyGraph is:"] ++ prettyCaleyGraph cg ++
     ["", "The complete list of its finite elements is:"] ++
      concatMap printRelWithCode (Set.toList wfs) ++
     ["", "The complete list of its infinite elements is:"] ++
      concatMap printRelWithCode (Set.toList nwfs)
     
easyPathReport :: Ord x => LabeledGraphI g x -> g -> IO ()
easyPathReport gi g = putStr . unlines $ (pathReport gi g)

untilNothing :: (a -> Maybe a) -> a -> [a]
untilNothing f g = let
    val = f g
  in case val of
       Nothing -> []
       Just x  -> x : untilNothing f x

takeTill :: (a -> Bool) -> [a] -> [a]
takeTill p [] = []
takeTill p (x:xs) = if p x then [x] else x : takeTill p xs

liftingReport :: Ord x => Int -> LabeledGraphI g x -> g -> [String]
liftingReport bound gi graph =
  let g = toLiftedGraph gi graph
      lI = liftedGraphIWithNodePrinter (LabeledGraph.prettyNode gi graph)
      lifts = take bound $ takeTill (hasDoubleRefl lI) $ untilNothing lift g
      printer gr = prettyPredLabeledGraph lI gr
      graphToSize g = Set.size $ LabeledGraph.domain lI g
  in  prettyLabeledGraph gi graph ++
      ["=============="] ++
      ["Size of the liftings: " ++ show (map graphToSize lifts)] ++
      intercalate [""] (map printer (take 2 $ lifts))

easyLiftingReport :: Ord x => Int -> LabeledGraphI g x -> g -> IO ()
easyLiftingReport b gi g = putStr . unlines $ (liftingReport b gi g)

liftingPathReport :: Ord x => Int -> LabeledGraphI g x -> g -> [String]
liftingPathReport bound gi graph =
  let
      lI = liftedGraphIWithNodePrinter (LabeledGraph.prettyNode gi graph)
      baseLiftedGraph = toLiftedGraph gi graph
      lifts = take bound $ takeTill (hasDoubleRefl lI) $ untilNothing (liftWithFilter dominationFilter) baseLiftedGraph
      fctGraph l bg = let wg = WG.WrappedGraph bg c undefined
                      in fctGraphFromDomain (LabeledGraph.domain liftedGraphINotPretty l) (liftedRelation (\x y -> Graph.hasArc (WG.wrappedGraphI (bitGraphI s)) wg (x,y)))
      printSuper l r = Graph.prettyGraph (fctGraphIWithNodePrinter (LabeledGraph.prettyNode lI l)) (fctGraph l r)
      printSuperRel l r = (printNodeWithSuccs cg r ++ ":") : (printSuper l r)
      cgRels = CaleyGraph.domain cg
      printLifting lg = intercalate [""] $ map (printSuperRel lg) (Set.toList cgRels)
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
      ["Size of the liftings: " ++ show (map graphToSize lifts)] ++
      intercalate ["", ""] (map printLifting lifts)

easyLiftingPathReport :: Ord x => Int -> LabeledGraphI g x -> g -> IO ()
easyLiftingPathReport b gi g = putStr . unlines $ (liftingPathReport b gi g)

