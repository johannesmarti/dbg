module WrappedGraph (
  WrappedGraph(..),
  wrappedGraphI,
) where

import qualified Data.Set as Set

import Coding
import Graph
import Pretty

data WrappedGraph g x y = WrappedGraph {
  innerGraph      :: g,
  coding          :: Coding y x,
  prettyOuterNode :: y -> String
}

wrappedGraphI :: (Ord x, Ord y) => GraphI g x
                                             -> GraphI (WrappedGraph g x y) y
wrappedGraphI innerI = interfaceFromAll dom succ pred hasAr ar prettyNode where
  dom wg = Coding.domain (coding wg)
  succ (WrappedGraph ig c _) node = Set.map (decode c) $
                                    successors innerI ig (encode c node)
  pred (WrappedGraph ig c _) node = Set.map (decode c) $
                                    predecessors innerI ig (encode c node)
  hasAr (WrappedGraph ig c _) (x,y) = hasArc innerI ig (encode c x, encode c y)
  ar (WrappedGraph ig c _) = Prelude.map (decodePair c)  $ arcs innerI ig
  decodePair c (x,y) = (decode c x, decode c y)
  prettyNode wg n = prettyOuterNode wg n
