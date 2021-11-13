module Bitify (
  bitify,
  caleyCondition,
  easyReport,
) where

import Control.Exception.Base
import Data.List (maximumBy)
import qualified Data.Set as Set

import BitGraph
import CaleyGraph
import Coding
import Graph
import UnlabeledBitGraph
import WrappedGraph

bitify :: Ord x => GraphI g x -> g -> WrappedGraph BitGraph Node x
bitify gi g = assert (wellDefined wrappedGraphI wrappedGraph) $ wrappedGraph where
  wrappedGraph = WrappedGraph bitGraphI bg c
  bg = fromArcs size newArcs
  c = fromAssoc assoc
  oldDom = Graph.domain gi g
  size = Set.size oldDom
  assoc = zip (Set.toList oldDom) [0 .. ]
  newArcs = map enc (arcs gi g)
  enc (u,l,v) = (aggressiveEncode c u, l, aggressiveEncode c v)

caleyCondition :: WrappedGraph BitGraph Node x -> Bool
caleyCondition wg = isReallyGood (size inner) cg where
  inner = innerGraph wg
  cg = caleyGraph inner

caleyReport :: (Ord x, Show x) => WrappedGraph BitGraph Node x -> [String]
caleyReport wg = let
    inner = innerGraph wg
    cg = caleyGraph inner
    s = size inner
    c = coding wg
    enc = aggressiveEncode c
    dec = aggressiveDecode c
    printRel r = prettyUnlabeled s (show . dec) r
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
      "It's finite words are:", show (map fst finWords),
      "Of which one with maximal length is " ++ show longestFinWord ++ ", " ++
      "which has relation:"] ++ (printRel relOfLongest) ++ ["",
      "Two of its infinit relations are:"] ++ printRel firstInfinite ++
     ["and"] ++ printRel lastInfinite
     

easyReport :: (Ord x, Show x) => GraphI g x -> g -> IO ()
easyReport gi g = putStr . unlines . caleyReport $ wg where
  wg = bitify gi g

