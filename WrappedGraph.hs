module WrappedGraph (
  WrappedGraph(..),
  wrappedGraphI,
) where

import qualified Data.Set as Set

import Coding
import Graph

data WrappedGraph g x y = WrappedGraph {
  innerInterface :: GraphI g x,
  innerGraph     :: g,
  coding         :: Coding y x
}

wrappedGraphI :: (Ord x, Ord y) => GraphI (WrappedGraph g x y) y
wrappedGraphI = GraphI dom succs preds

dom :: Ord y => WrappedGraph g x y -> Set.Set y
dom wg = Coding.domain (coding wg)

succs :: (Ord x, Ord y) => WrappedGraph g x y -> MapFunction y
succs (WrappedGraph iI ig c) label node = Set.map (aggressiveDecode c) $
                                            successors iI ig label (aggressiveEncode c node)

preds :: (Ord x, Ord y) => WrappedGraph g x y -> MapFunction y
preds (WrappedGraph iI ig c) label node = Set.map (aggressiveDecode c) $
                                            predecessors iI ig label (aggressiveEncode c node)

instance (Ord x, Ord y, Show y) => Show (WrappedGraph g x y) where
  show = unlines . (Graph.prettyGraph wrappedGraphI)
