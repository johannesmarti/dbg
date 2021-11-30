module Reports (
  pathReport, easyPathReport,
  liftingReport, easyLiftingReport,
) where

import qualified Data.Set as Set
import Data.List (maximumBy,intercalate)

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
    firstInfinite = Set.findMin nwfs
    lastInfinite = Set.findMax nwfs
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
      intercalate [""] (map printer lifts)

easyLiftingReport :: Ord x => Int -> LabeledGraphI g x -> g -> IO ()
easyLiftingReport b gi g = putStr . unlines $ (liftingReport b gi g)

liftingPathReport :: Ord x => LabeledGraphI g x -> g -> [String]
liftingPathReport = undefined
