module Bitable (
  Bitification(..),
  BitableI,
  liftedGraphBitableI,
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
  reflexivesUniversalInMultiple :: BitGraph -> Set.Set x,
  relationI :: GraphI BitGraph x
}

type BitableI g x = g -> Bitification x

lBitGraphBitableI :: Size -> BitableI LBitGraph Node
lBitGraphBitableI s lbg =
  Bitification s lbg (reflexivesUnivInMultiple s) (bitGraphI s)

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
  c = coding wg
  decodePair (x,y) = (decode c x, decode c y)
  bgi = bitGraphI s
  rum bg = Set.map (decode c) $ reflexivesUnivInMultiple s bg
  outTypeI = interfaceFromAll (\_ -> LG.domain (lWrappedGraphI (lBitGraphI s)) wg)
                              (\bg node -> Set.map (decode c) $
                                            successors bgi bg (encode c node))
                              (\bg node -> Set.map (decode c) $
                                            predecessors bgi bg (encode c node))
                              (\bg (x,y) -> hasArc bgi bg (encode c x, encode c y))
                              (\bg -> Prelude.map decodePair $ arcs bgi bg)
                              (\bg node -> prettyOuterNode wg node)
  bitification = Bitification s (innerGraph wg) rum outTypeI
