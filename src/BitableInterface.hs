module BitableInterface (
  Bitification(..),
  BitableInterface,
  combinationGraphBitableInterface,
  conciseGraphBitableInterface,
  genericBitableInterface,
) where

{-
This code is the interface that allows code, such as for instance the Cayley
graph module or all the RelationCache code, to access to a bit representation
of a generic graph. The code solves two problems that might not need solving or
that may better be solved differently.
1) In case the original graph is already stored in a bit representation, such
as LabeledBitGraph or ConcieseGraph, we want to provide access more or less
directely, without wrapping the graph in a LabeledWrapped Graph. This might be
a premature optimization. It is unclear what the performance benefits as
opposed to trivial instances of LabeledWrappedGraph.
2) The code also provides an interface for the relations of individual
(unlabeled) relations that are computed for words in the original graph. This
is the 'relationI :: GraphInterface BitGraph x' field below. The purpose is to
allow easy access to such an interface that forgets about the labeling. I did
not yet figure out how to do that nicely from a LabeledWrappedGraph. But I
suspect there must be a more elegant solution.

It might also be that these problems don't need to be solved! We might also be
happy to only be able to turn BitGraphs into CayleyGraphs. Thus, the more
general issue to provide CayleyGraphs for arbitrary graphs might just be
overkill.
-}

import Bitify
import Graphs.BitGraph
import Coding
import Graphs.ConciseGraph
import Graphs.CommonLabeledGraphTypes
import Graphs.GraphInterface
import Lifting.CombinationGraph
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

combinationGraphBitableInterface :: BitableInterface (CombinationGraph x) Int
combinationGraphBitableInterface lg = bitification where
  (lbg,s) = Lifting.CombinationGraph.toLabeledBitGraph lg
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
