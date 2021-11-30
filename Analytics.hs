module Analytics (
  pathReport,
  easyPathReport,
) where

import qualified Data.Set as Set
import Data.List (maximumBy)

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

liftingReport :: Ord x => LabeledGraphI g x -> g -> [String]
liftingReport gi graph =
  let g = toLiftedGraph gi graph
      lifts = takeTill (hasDoubleRefl liftedGraphI) $ untilNothing lift g
      printer graph = putStrLn $ unlines $ prettyPredLabeledGraph liftedGraphI graph
      graphToSize g = Set.size $ LabeledGraph.domain liftedGraphI g
  in do putStr $ unlines $ prettyLabeledGraph gi graph
        putStrLn "=============="
        mapM_ printer (take 3 lifts)
        mapM_ (print . graphToSize) (take 5 $ lifts)

easyliftingReport :: Ord x => LabeledGraphI g x -> g -> IO ()
easyliftingReport gi g = putStr . unlines $ (pathReport gi g)

liftingPathReport :: Ord x => LabeledGraphI g x -> g -> [String]
liftingPathReport = undefined
