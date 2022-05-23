module Bitify (
  BityGraph, bityGraphI,
  LBityGraph, lBityGraphI,
  bitify,
  labeledBitify,
  toConcise,
  hasPathCondition,
) where

import Control.Exception.Base
import Data.List (maximumBy)
import qualified Data.Set as Set

import CommonLGraphTypes
import BitGraph
import Coding
import ConciseGraph
import Graph
import LabeledGraph
import WrappedGraph
import LWrappedGraph
import PairGraph
import CayleyGraph

type BityGraph x = WrappedGraph BitGraph Node x
type LBityGraph x = LWrappedGraph LBitGraph Node x

bityGraphI :: Ord x => Size -> GraphI (BityGraph x) x
bityGraphI s = wrappedGraphI (bitGraphI s)

lBityGraphI :: Ord x => Size -> LabeledGraphI (LBityGraph x) x
lBityGraphI s = lWrappedGraphI (lBitGraphI s)

bitify :: Ord x => GraphI g x -> g -> (BityGraph x, Size)
bitify gi g = (wrappedGraph,size) where
  wrappedGraph = WrappedGraph bg c printer
  bg = BitGraph.fromArcs size newArcs
  oldDom = Graph.domain gi g
  c = codeSet oldDom
  printer = Graph.prettyNode gi g
  size = Set.size oldDom
  newArcs = map enc (Graph.arcs gi g)
  enc (u,v) = (encode c u, encode c v)

labeledBitify :: Ord x => LabeledGraphI g x -> g -> (LBityGraph x, Size)
labeledBitify gi g = (wrappedGraph, size) where
  wrappedGraph = LWrappedGraph lbg c printer
  bitGraphPerLabel l = BitGraph.fromArcs size (newArcs l)
  lbg = PairGraph.fromFunction bitGraphPerLabel
  oldDom = LabeledGraph.domain gi g
  c = codeSet oldDom
  printer = LabeledGraph.prettyNode gi g
  size = Set.size oldDom
  newArcs l = map enc (LabeledGraph.arcsOfLabel gi g l)
  enc (u,v) = (encode c u, encode c v)

toConcise :: Ord x => LabeledGraphI g x -> g -> (Size, ConciseGraph)
toConcise gi g = (s,cg) where
  (wg,s) = labeledBitify gi g
  cg = fromLBitGraph s (LWrappedGraph.innerGraph wg)

hasPathCondition :: Ord x => LabeledGraphI g x -> g -> Bool
hasPathCondition gi g = pathCondition size cg where
  (wg, size) = labeledBitify gi g
  inner = LWrappedGraph.innerGraph wg
  cg = cayleyGraphOfLBitGraph size inner
