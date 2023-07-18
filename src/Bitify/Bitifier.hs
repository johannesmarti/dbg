module Bitify.Bitifier (
  Bitification(numBits,labeledBitGraph,coding,interface,relationInterface),
  Bitifier,
  labeledBitGraphBitifier,
  combinationGraphBitifier,
  conciseGraphBitifier,
  genericBitifier,
  toConcise,
) where

import Control.Exception.Base
import Data.List (maximumBy)
import qualified Data.Set as Set

import Graphs.CommonLabeledGraphTypes
import Graphs.BitGraph
import Graphs.ConciseGraph
import Graphs.GraphInterface as GI
import Graphs.LabeledGraphInterface as LGI
import Graphs.PairGraph
import Lifting.CombinationGraph
import Bitify.Coding as C

data Bitification x = Bitification {
  numBits           :: Size,
  labeledBitGraph   :: LabeledBitGraph,
  coding            :: Coding x Node,
  interface         :: LabeledGraphInterface LabeledBitGraph x,
  relationInterface :: GraphInterface BitGraph x
}

bitify :: Ord x => LabeledGraphInterface g x -> g -> Bitification x
bitify gi g = Bitification s lbg c iface relIface where
  oldDomain = LGI.domain gi g
  s = Set.size oldDomain
  c = codeSet oldDomain
  enc (u,v) = (encode c u, encode c v)
  newArcs l = map enc (LGI.arcsOfLabel gi g l)
  bitGraphPerLabel l = Graphs.BitGraph.fromArcs s (newArcs l)
  lbg = Graphs.PairGraph.fromFunction bitGraphPerLabel
  printer = LGI.prettyNode gi g
  lbgi = labeledBitGraphInterface s
  iface = LGI.interfaceFromAll 
               (\lbg -> C.domain c)
               (\lbg l node -> (decodeSet c) $
                                 LGI.successors lbgi lbg l (encode c node))
               (\lbg l node -> (decodeSet c) $
                                 LGI.predecessors lbgi lbg l (encode c node))
               (\lbg l arc -> LGI.hasArc lbgi lbg l (encodeArc c arc))
               (\lbg l -> (decodeArcs c) $ LGI.arcsOfLabel lbgi lbg l)
               (\lbg node -> printer node)
  bgi = bitGraphInterface s
  relIface = GI.interfaceFromAll
               (\bg -> C.domain c)
               (\bg node -> (decodeSet c) $
                              GI.successors bgi bg (encode c node))
               (\bg node -> (decodeSet c) $
                              GI.predecessors bgi bg (encode c node))
               (\bg arc -> GI.hasArc bgi bg (encodeArc c arc))
               (\bg -> (decodeArcs c) $ GI.arcs bgi bg)
               (\bg node -> printer node)


type Bitifier g x = g -> Bitification x

labeledBitGraphBitifier :: Size -> Bitifier LabeledBitGraph Node
labeledBitGraphBitifier s lbg =
  Bitification s lbg (identityCoding (Graphs.BitGraph.nodesSet s))
                     (labeledBitGraphInterface s)
                     (bitGraphInterface s)

conciseGraphBitifier :: Size -> Bitifier ConciseGraph Node
conciseGraphBitifier s cg = bitification where
  lbg = Graphs.ConciseGraph.toLabeledBitGraph s cg
  bitification = labeledBitGraphBitifier s lbg

combinationGraphBitifier :: Bitifier (CombinationGraph x) Int
combinationGraphBitifier lg = bitification where
  (lbg,s) = Lifting.CombinationGraph.toLabeledBitGraph lg
  bitification = labeledBitGraphBitifier s lbg

genericBitifier :: Ord x => LGI.LabeledGraphInterface g x -> Bitifier g x
genericBitifier gi g = bitify gi g

toConcise :: Ord x => LabeledGraphInterface g x -> g -> (Size, ConciseGraph)
toConcise gi g = (s,cg) where
  bitification = bitify gi g
  s = numBits bitification
  cg = fromLabeledBitGraph s (labeledBitGraph bitification)
