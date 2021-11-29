module Analytics (
  pathReport,
  easyReport,
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

pathReport :: (Ord x, Pretty x) => Size -> LWrappedGraph LBitGraph Node x -> [String]
pathReport s wg = let
    inner = innerGraph wg
    cg = caleyGraphOfLBitGraph s inner
    c = coding wg
    enc = encode c
    dec = decode c
    printRel r = Graph.prettyGraph (WG.wrappedGraphI (bitGraphI s)) (WG.WrappedGraph r c)
    printRelWithCode r = (printNodeWithSuccs cg r ++ ":") : (printRel r)
    wfs = wellfoundedElements cg
    nwfs = nonWellfoundedElements cg
    finWords = finiteWords s cg
    (longestFinWord,relOfLongest) = maximumBy (\(a,_) (b,_) -> compare (length a) (length b)) finWords
    firstInfinite = Set.findMin nwfs
    lastInfinite = Set.findMax nwfs
  in ["About the Caley-graph of the pattern:",
      show wg,
      "It has " ++ show (Set.size wfs) ++ " finite and " ++
                   show (Set.size nwfs) ++ " infinite elements.", "",
      "It " ++ (if isGood s cg then "satisfies" else "does not satisfy") ++ " the path condition.", "",
      "It's finite words are:", show (map fst finWords),
      "Of which one with maximal length is " ++ show longestFinWord ++ ", " ++
      "which has relation:"] ++ (printRel relOfLongest) ++ ["",
      "Two of its infinite relations are:"] ++ printRel firstInfinite ++
     ["and"] ++ printRel lastInfinite ++ [""] ++
     ["The full CaleyGraph is:"] ++ prettyCaleyGraph cg ++
     ["", "The complete list of its finite elements is:"] ++
      concatMap printRelWithCode (Set.toList wfs) ++
     ["", "The complete list of its infinite elements is:"] ++
      concatMap printRelWithCode (Set.toList nwfs)
     

easyReport :: (Ord x, Pretty x) => LabeledGraphI g x -> g -> IO ()
easyReport gi g = putStr . unlines $ (pathReport size wg) where
  (wg,size) = labeledBitify gi g
