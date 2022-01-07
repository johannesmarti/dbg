module Bitify (
  bitify,
  labeledBitify,
  hasPathCondition,
) where

import Control.Exception.Base
import Data.List (maximumBy)
import qualified Data.Set as Set

import CommonLGraphTypes
import BitGraph
import Coding
import Graph
import LabeledGraph
import WrappedGraph
import LWrappedGraph
import PairGraph
import CaleyGraph

setToCoding :: Ord x => Set.Set x -> Coding x Int
setToCoding set = fromAssoc assoc where
  assoc = zip (Set.toList set) [0 .. ]

bitify :: Ord x => GraphI g x -> g -> (WrappedGraph BitGraph Node x, Size)
bitify gi g = (wrappedGraph,size) where
  wrappedGraph = WrappedGraph bg c printer
  bg = BitGraph.fromArcs size newArcs
  oldDom = Graph.domain gi g
  c = setToCoding oldDom
  printer = Graph.prettyNode gi g
  size = Set.size oldDom
  newArcs = map enc (Graph.arcs gi g)
  enc (u,v) = (encode c u, encode c v)

labeledBitify :: Ord x => LabeledGraphI g x -> g -> (LWrappedGraph LBitGraph Node x, Size)
labeledBitify gi g = (wrappedGraph, size) where
  wrappedGraph = LWrappedGraph lbg c printer
  bitGraphPerLabel l = BitGraph.fromArcs size (newArcs l)
  lbg = PairGraph.fromFunction bitGraphPerLabel
  oldDom = LabeledGraph.domain gi g
  c = setToCoding oldDom
  printer = LabeledGraph.prettyNode gi g
  size = Set.size oldDom
  newArcs l = map enc (LabeledGraph.arcsOfLabel gi g l)
  enc (u,v) = (encode c u, encode c v)

hasPathCondition :: Ord x => LabeledGraphI g x -> g -> Bool
hasPathCondition gi g = isGood size cg where
  (wg, size) = labeledBitify gi g
  inner = LWrappedGraph.innerGraph wg
  cg = caleyGraphOfLBitGraph size inner
