module Bitify (
  bitify,
) where

import Control.Exception.Base
import qualified Data.Set as Set

import BitGraph
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
