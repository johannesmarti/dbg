module Bitify (
  bitify,
  labeledBitify,
) where

import Control.Exception.Base
import Data.List (maximumBy)
import qualified Data.Set as Set

import CommonLGraphTypes
import BitGraph
import CaleyGraph
import Coding
import Graph
import LabeledGraph
import WrappedGraph
import LWrappedGraph
import PairGraph

setToCoding :: Ord x => Set.Set x -> Coding x Int
setToCoding set = fromAssoc assoc where
  assoc = zip (Set.toList set) [0 .. ]

bitify :: Ord x => GraphI g x -> g -> (WrappedGraph BitGraph Node x, Size)
bitify gi g = (wrappedGraph,size) where
  wrappedGraph = WrappedGraph bg c
  bg = BitGraph.fromArcs size newArcs
  oldDom = Graph.domain gi g
  c = setToCoding oldDom
  size = Set.size oldDom
  newArcs = map enc (Graph.arcs gi g)
  enc (u,v) = (encode c u, encode c v)

labeledBitify :: Ord x => LabeledGraphI g x -> g -> (LWrappedGraph LBitGraph Node x, Size)
labeledBitify gi g = (wrappedGraph, size) where
  wrappedGraph = LWrappedGraph lbg c
  bitGraphPerLabel l = BitGraph.fromArcs size (newArcs l)
  lbg = PairGraph.fromFunction bitGraphPerLabel
  oldDom = LabeledGraph.domain gi g
  c = setToCoding oldDom
  size = Set.size oldDom
  newArcs l = map enc (LabeledGraph.arcsOfLabel gi g l)
  enc (u,v) = (encode c u, encode c v)

{-
labeledBitify gi g = assert (wellDefined wrappedGraphI wrappedGraph) $ wrappedGraph where
  wrappedGraph = WrappedGraph bitGraphI bg c
  bg = fromArcs size newArcs
  c = fromAssoc assoc
  oldDom = Graph.domain gi g
  size = Set.size oldDom
  assoc = zip (Set.toList oldDom) [0 .. ]
  newArcs = map enc (arcs gi g)
  enc (u,l,v) = (aggressiveEncode c u, l, aggressiveEncode c v)
-}

{-
caleyCondition :: WrappedGraph BitGraph Node x -> Bool
caleyCondition wg = isReallyGood (size inner) cg where
  inner = innerGraph wg
  cg = caleyGraph inner

pathReport :: (Ord x, Show x) => WrappedGraph BitGraph Node x -> [String]
pathReport wg = let
    inner = innerGraph wg
    cg = caleyGraph inner
    s = size inner
    c = coding wg
    enc = aggressiveEncode c
    dec = aggressiveDecode c
    printRel r = prettyUnlabeled s (show . dec) r
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
      "It " ++ (if isReallyGood s cg then "satisfies" else "does not satisfy") ++ " the path condition.", "",
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
     

easyReport :: (Ord x, Show x) => GraphI g x -> g -> IO ()
easyReport gi g = putStr . unlines . pathReport $ wg where
  wg = bitify gi g
-}
