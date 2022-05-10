module Report (
  pathReport, easyPathReport,
  wordReport, easyWordReport,
  liftingReport, easyLiftingReport,
  liftingPathReport, easyLiftingPathReport,
) where

import qualified Data.Set as Set
import Data.List (maximumBy,intercalate)

import FctGraph
import Bitify
import LWrappedGraph
import qualified WrappedGraph as WG
import qualified Path
import LabeledGraph
import CommonLGraphTypes
import CayleyGraph
import BitGraph
import Coding
import Pretty
import Graph as Graph
import Lifting
import DeterminismProperty
import PathTree

import Tools

pathReport :: Ord x => LabeledGraphI g x -> g -> [String]
pathReport gi g = let
    (wg,s) = labeledBitify gi g
    inner = innerGraph wg
    cg = cayleyGraphOfLBitGraph s inner
    c = coding wg
    enc = encode c
    dec = decode c
    printRel r = Graph.prettyGraph (WG.wrappedGraphI (bitGraphI s)) (WG.WrappedGraph r c (LabeledGraph.prettyNode gi g))
    printRelWithCode r = (printNodeWithSuccs cg r ++ ":") : (printRel r)
    wfs = wellfoundedElements cg
    nwfs = nonWellfoundedElements cg
    finWords = finiteWords s cg
    (longestFinWord,relOfLongest) = maximumBy (\(a,_) (b,_) -> compare (length a) (length b)) finWords
  in ["About the Cayley graph of the pattern:"] ++
      LabeledGraph.prettyLabeledGraph gi g ++
     ["It has " ++ show (Set.size wfs) ++ " finite and " ++
                   show (Set.size nwfs) ++ " infinite elements.", "",
      "It " ++ (if isConstructionDeterministic gi g then "is" else "is not") ++ " construction deterministic.", "",
      "It " ++ (if pathCondition s cg then "satisfies" else "does not satisfy") ++ " the path condition.", "",
      --"It's finite words are:", show (map fst finWords),
      "Of which one with maximal length is " ++ show longestFinWord ++ ".", ""] ++
     ["The full CayleyGraph is:"] ++ prettyCayleyGraph cg ++
     ["", "The complete list of its finite elements is:"] ++
      concatMap printRelWithCode (Set.toList wfs) ++
     ["", "The complete list of its infinite elements is:"] ++
      concatMap printRelWithCode (Set.toList nwfs)
     
easyPathReport :: Ord x => LabeledGraphI g x -> g -> IO ()
easyPathReport gi g = putStr . unlines $ (pathReport gi g)

liftingReport :: Ord x => Int -> LabeledGraphI g x -> g -> [String]
liftingReport bound gi graph =
  let g = toLiftedGraph gi graph
      lI = liftedGraphIWithNodePrinter (LabeledGraph.prettyNode gi graph)
      lFilter = weakDominationFilter
      --lFilter = noFilter
      lifts = take bound $ takeTill (hasT1 lI) $ untilNothing (liftWithFilter lFilter) g
      --printer gr = prettyPredLabeledGraph lI gr
      printer gr = prettyBigLabeledGraph lI gr
      graphToSize g = Set.size $ LabeledGraph.domain lI g
  in  prettyLabeledGraph gi graph ++
      ["Size of the liftings: " ++ show (map graphToSize lifts), "", "==========", ""] ++
      intercalate ["", "+++++++++++++", ""] (map printer lifts)

easyLiftingReport :: Ord x => Int -> LabeledGraphI g x -> g -> IO ()
easyLiftingReport b gi g = putStr . unlines $ (liftingReport b gi g)

wordReport :: Ord x => Int -> LabeledGraphI g x -> g -> [String]
wordReport numWords gi g = let
    (wg,s) = labeledBitify gi g
    inner = innerGraph wg
    cg = cayleyGraphOfLBitGraph s inner
    c = coding wg
    enc = encode c
    dec = decode c
    wordToRel = relationOfWord s cg
    printNode = (LabeledGraph.prettyNode gi g) . dec
    wrapI = WG.wrappedGraphI (bitGraphI s)
    printRel r = Graph.prettyGraph wrapI (WG.WrappedGraph r c (LabeledGraph.prettyNode gi g))
    printRelWithCode r = (printNodeWithSuccs cg r ++ ":") : (printRel r)
    printWordWithRel (w,r) = [show w ++ ":"] ++ printRelWithCode r
    printWordWithRelAndPathes (w,r) = let
        cycles = concatMap pathesOnPathTree $
                   pathTreesOfMCycles (inner,s) wordToRel w
      in printWordWithRel (w,r) ++ map (Path.prettyPath printNode) cycles
    wfs = wellfoundedElements cg
    nwfs = nonWellfoundedElements cg
    finWords = map fst $ finiteWords s cg
    longestFinWord = maximumBy (\a b -> compare (length a) (length b)) finWords
    wordRels = take numWords $ allWords s cg
  in ["About the Cayley graph of the pattern:"] ++
      LabeledGraph.prettyLabeledGraph gi g ++
     ["It has " ++ show (Set.size wfs) ++ " finite and " ++
                   show (Set.size nwfs) ++ " infinite elements.", "",
      "It " ++ (if isConstructionDeterministic gi g then "is" else "is not") ++ " construction deterministic.", "",
      "It " ++ (if pathCondition s cg then "satisfies" else "does not satisfy") ++ " the path condition.", ""] ++ 
--      "It's finite words are:", show finWords,
--      "Of which one with maximal length is " ++ show longestFinWord ++ ".", ""] ++
     ["The relations of the first " ++ show numWords ++ " words are:"] ++
      --intercalate [""] (map printWordWithRel wordRels)
      intercalate [""] (map printWordWithRelAndPathes wordRels)

easyWordReport :: Ord x => Int -> LabeledGraphI g x -> g -> IO ()
easyWordReport numWords gi g = putStr . unlines $ (wordReport numWords gi g)

liftingPathReport :: Ord x => Int -> LabeledGraphI g x -> g -> [String]
liftingPathReport bound gi graph =
  let g = toLiftedGraph gi graph
      lI = liftedGraphIWithNodePrinter (LabeledGraph.prettyNode gi graph)
      --fi = weakDominationFilter
      --fi = dominationFilter
      fi = noFilter
      lifts = take bound $ takeTill (hasT1 lI) $ untilNothing (liftWithFilter fi) g
      --lReport iface gra = wordReport 7 iface gra
      lReport iface gra = pathReport iface gra
      printer g = lReport lI g
      graphToSize g = Set.size $ LabeledGraph.domain lI g
  in  lReport gi graph ++
      ["=============="] ++
      ["Size of the liftings: " ++ show (map graphToSize lifts)] ++
      intercalate ["", "+++++++++++++", ""] (map printer lifts)

easyLiftingPathReport :: Ord x => Int -> LabeledGraphI g x -> g -> IO ()
easyLiftingPathReport b gi g = putStr . unlines $ (liftingPathReport b gi g)

