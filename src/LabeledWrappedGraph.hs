module LabeledWrappedGraph (
  LabeledWrappedGraph(..),
  labeledWrappedGraphInterface,
) where

import qualified Data.Set as Set

import Coding
import Graphs.LabeledGraphInterface

data LabeledWrappedGraph g x y = LabeledWrappedGraph {
  innerGraph :: g,
  coding     :: Coding y x,
  prettyOuterNode :: y -> String
}

labeledWrappedGraphInterface :: (Ord x, Ord y) => LabeledGraphInterface g x
                                              -> LabeledGraphInterface (LabeledWrappedGraph g x y) y
labeledWrappedGraphInterface innerI = iFromAll dom succ pred hasAr ar prettyNode where
  dom wg = Coding.domain (coding wg)
  succ (LabeledWrappedGraph ig c _) l node = (decodeSet c) $
                                      successors innerI ig l (encode c node)
  pred (LabeledWrappedGraph ig c _) l node = (decodeSet c) $
                                      predecessors innerI ig l (encode c node)
  hasAr (LabeledWrappedGraph ig c _) l a = hasArc innerI ig l (encodeArc c a)
  ar (LabeledWrappedGraph ig c _) l = (decodeArcs c) $ arcsOfLabel innerI ig l
  prettyNode wg n = prettyOuterNode wg n
