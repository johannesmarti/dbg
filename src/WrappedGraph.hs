module WrappedGraph (
  WrappedGraph(..),
  wrappedGraphInterface,
) where

import qualified Data.Set as Set

import Coding
import GraphInterface

data WrappedGraph g x y = WrappedGraph {
  innerGraph      :: g,
  coding          :: Coding y x,
  prettyOuterNode :: y -> String
}

wrappedGraphInterface :: (Ord x, Ord y) => GraphInterface g x -> GraphInterface (WrappedGraph g x y) y
wrappedGraphInterface innerI = interfaceFromAll dom succ pred hasAr ar prettyNode where
  dom wg = Coding.domain (coding wg)
  succ (WrappedGraph ig c _) node = (decodeSet c) $
                                    successors innerI ig (encode c node)
  pred (WrappedGraph ig c _) node = (decodeSet c) $
                                    predecessors innerI ig (encode c node)
  hasAr (WrappedGraph ig c _) (x,y) = hasArc innerI ig (encode c x, encode c y)
  ar (WrappedGraph ig c _) = (decodeArcs c) $ arcs innerI ig
  prettyNode wg n = prettyOuterNode wg n
