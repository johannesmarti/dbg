module BitableInterface (
  Bitification(..),
  BitableInterface,
  liftedGraphBitableInterface,
  conciseGraphBitableInterface,
  genericBitableInterface,
) where

import Bitify
import Graphs.BitGraph
import Coding
import Graphs.ConciseGraph
import Graphs.CommonLabeledGraphTypes
import Graphs.GraphInterface
import LiftedGraph
import qualified Graphs.LabeledGraphInterface as LGI
import LabeledWrappedGraph

data Bitification x = Bitification {
  numBits :: Size,
  labeledBitGraph :: LabeledBitGraph,
  coding :: Coding x Node,
  relationI :: GraphInterface BitGraph x
}

type BitableInterface g x = g -> Bitification x

labeledBitGraphBitableInterface :: Size -> BitableInterface LabeledBitGraph Node
labeledBitGraphBitableInterface s lbg =
  Bitification s lbg (identityCoding (Graphs.BitGraph.nodesSet s)) (bitGraphInterface s)

conciseGraphBitableInterface :: Size -> BitableInterface ConciseGraph Node
conciseGraphBitableInterface s cg = bitification where
  lbg = Graphs.ConciseGraph.toLabeledBitGraph s cg
  bitification = labeledBitGraphBitableInterface s lbg

liftedGraphBitableInterface :: BitableInterface (LiftedGraph x) Int
liftedGraphBitableInterface lg = bitification where
  (lbg,s) = LiftedGraph.toLabeledBitGraph lg
  bitification = labeledBitGraphBitableInterface s lbg

genericBitableInterface :: Ord x => LGI.LabeledGraphInterface g x -> BitableInterface g x
genericBitableInterface gi g = bitification where
  (wg,s) = labeledBitify gi g
  c = LabeledWrappedGraph.coding wg
  bgi = bitGraphInterface s
  outTypeI = interfaceFromAll (\_ -> LGI.domain (labeledWrappedGraphInterface (labeledBitGraphInterface s)) wg)
                              (\bg node -> (decodeSet c) $
                                            successors bgi bg (encode c node))
                              (\bg node -> (decodeSet c) $
                                            predecessors bgi bg (encode c node))
                              (\bg arc -> hasArc bgi bg (encodeArc c arc))
                              (\bg -> (decodeArcs c) $ arcs bgi bg)
                              (\bg node -> prettyOuterNode wg node)
  bitification = Bitification s (innerGraph wg) c outTypeI
