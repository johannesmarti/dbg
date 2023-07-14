module Bitify (
  BityGraph, bityGraphInterface,
  LabeledBityGraph, labeledBityGraphInterface,
  bitify,
  labeledBitify,
  toConcise,
) where

import Control.Exception.Base
import Data.List (maximumBy)
import qualified Data.Set as Set

import CommonLabeledGraphTypes
import BitGraph
import Coding
import ConciseGraph
import GraphInterface
import LabeledGraphInterface as LGI
import WrappedGraph
import LabeledWrappedGraph
import PairGraph

type BityGraph x = WrappedGraph BitGraph Node x
type LabeledBityGraph x = LabeledWrappedGraph LabeledBitGraph Node x

bityGraphInterface :: Ord x => Size -> GraphInterface (BityGraph x) x
bityGraphInterface s = wrappedGraphInterface (bitGraphInterface s)

labeledBityGraphInterface :: Ord x => Size -> LabeledGraphInterface (LabeledBityGraph x) x
labeledBityGraphInterface s = labeledWrappedGraphInterface (labeledBitGraphInterface s)

bitify :: Ord x => GraphInterface g x -> g -> (BityGraph x, Size)
bitify gi g = (wrappedGraph,size) where
  wrappedGraph = WrappedGraph bg c printer
  bg = BitGraph.fromArcs size newArcs
  oldDom = GraphInterface.domain gi g
  c = codeSet oldDom
  printer = GraphInterface.prettyNode gi g
  size = Set.size oldDom
  newArcs = map enc (GraphInterface.arcs gi g)
  enc (u,v) = (encode c u, encode c v)

labeledBitify :: Ord x => LabeledGraphInterface g x -> g -> (LabeledBityGraph x, Size)
labeledBitify gi g = (wrappedGraph, size) where
  wrappedGraph = LabeledWrappedGraph lbg c printer
  bitGraphPerLabel l = BitGraph.fromArcs size (newArcs l)
  lbg = PairGraph.fromFunction bitGraphPerLabel
  oldDom = LGI.domain gi g
  c = codeSet oldDom
  printer = LGI.prettyNode gi g
  size = Set.size oldDom
  newArcs l = map enc (LGI.arcsOfLabel gi g l)
  enc (u,v) = (encode c u, encode c v)

toConcise :: Ord x => LabeledGraphInterface g x -> g -> (Size, ConciseGraph)
toConcise gi g = (s,cg) where
  (wg,s) = labeledBitify gi g
  cg = fromLabeledBitGraph s (LabeledWrappedGraph.innerGraph wg)
