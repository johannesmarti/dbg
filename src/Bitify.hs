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

import Graphs.CommonLabeledGraphTypes
import Graphs.BitGraph
import Coding
import Graphs.ConciseGraph
import Graphs.GraphInterface as GI
import Graphs.LabeledGraphInterface as LGI
import WrappedGraph
import LabeledWrappedGraph
import Graphs.PairGraph

type BityGraph x = WrappedGraph BitGraph Node x
type LabeledBityGraph x = LabeledWrappedGraph LabeledBitGraph Node x

bityGraphInterface :: Ord x => Size -> GraphInterface (BityGraph x) x
bityGraphInterface s = wrappedGraphInterface (bitGraphInterface s)

labeledBityGraphInterface :: Ord x => Size -> LabeledGraphInterface (LabeledBityGraph x) x
labeledBityGraphInterface s = labeledWrappedGraphInterface (labeledBitGraphInterface s)

bitify :: Ord x => GraphInterface g x -> g -> (BityGraph x, Size)
bitify gi g = (wrappedGraph,size) where
  wrappedGraph = WrappedGraph bg c printer
  bg = Graphs.BitGraph.fromArcs size newArcs
  oldDom = GI.domain gi g
  c = codeSet oldDom
  printer = GI.prettyNode gi g
  size = Set.size oldDom
  newArcs = map enc (GI.arcs gi g)
  enc (u,v) = (encode c u, encode c v)

labeledBitify :: Ord x => LabeledGraphInterface g x -> g -> (LabeledBityGraph x, Size)
labeledBitify gi g = (wrappedGraph, size) where
  wrappedGraph = LabeledWrappedGraph lbg c printer
  bitGraphPerLabel l = Graphs.BitGraph.fromArcs size (newArcs l)
  lbg = Graphs.PairGraph.fromFunction bitGraphPerLabel
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
