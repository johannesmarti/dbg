module Bitable (
  Bitification(..),
  BitableI,
  liftedGraphBitableI,
  conciseGraphBitableI,
  genericBitableI,
) where

import Data.Set as Set

import Bitify
import BitGraph
import Coding
import ConciseGraph
import CommonLGraphTypes
import Graph
import LiftedGraph
import qualified LabeledGraph as LG
import RelationTree
import WordTree (labelOfWord)
import LWrappedGraph

data Bitification x = Bitification {
  numBits :: Size,
  labeledBitGraph :: LBitGraph,
  coding :: Coding x Node,
  relationI :: GraphI BitGraph x
}

type BitableI g x = g -> Bitification x

lBitGraphBitableI :: Size -> BitableI LBitGraph Node
lBitGraphBitableI s lbg =
  Bitification s lbg (identityCoding (BitGraph.nodesSet s)) (bitGraphI s)

conciseGraphBitableI :: Size -> BitableI ConciseGraph Node
conciseGraphBitableI s cg = bitification where
  lbg = ConciseGraph.toLBitGraph s cg
  bitification = lBitGraphBitableI s lbg

liftedGraphBitableI :: BitableI (LiftedGraph x) Int
liftedGraphBitableI lg = bitification where
  (lbg,s) = LiftedGraph.toLBitGraph lg
  bitification = lBitGraphBitableI s lbg

genericBitableI :: Ord x => LG.LabeledGraphI g x -> BitableI g x
genericBitableI gi g = bitification where
  (wg,s) = labeledBitify gi g
  c = LWrappedGraph.coding wg
  bgi = bitGraphI s
  outTypeI = interfaceFromAll (\_ -> LG.domain (lWrappedGraphI (lBitGraphI s)) wg)
                              (\bg node -> (decodeSet c) $
                                            successors bgi bg (encode c node))
                              (\bg node -> (decodeSet c) $
                                            predecessors bgi bg (encode c node))
                              (\bg arc -> hasArc bgi bg (encodeArc c arc))
                              (\bg -> (decodeArcs c) $ arcs bgi bg)
                              (\bg node -> prettyOuterNode wg node)
  bitification = Bitification s (innerGraph wg) c outTypeI
