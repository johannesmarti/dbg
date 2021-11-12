module Bitify (
  bitify,
  caleyCondition,
  easyReport,
) where

import Control.Exception.Base
import qualified Data.Set as Set

import BitGraph
import CaleyGraph
import Coding
import Graph
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
    cg = caleyGraph (innerGraph wg)
    c = coding wg
    enc = aggressiveEncode c
    dec = aggressiveDecode c
    wfs = wellfoundedElements cg
    nwfs = nonWellfoundedElements cg
  in ["About the Caley-graph of the pattern:",
      show wg,
      "It has " ++ show (Set.size wfs) ++ " finite and " ++
                   show (Set.size nwfs) ++ " infinite elements."]

easyReport :: (Ord x, Show x) => GraphI g x -> g -> IO ()
easyReport gi g = putStr . unlines . caleyReport $ wg where
  wg = bitify gi g

