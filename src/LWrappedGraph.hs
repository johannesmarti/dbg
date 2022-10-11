module LWrappedGraph (
  LWrappedGraph(..),
  lWrappedGraphI,
) where

import qualified Data.Set as Set

import Coding
import LabeledGraph
import Pretty

data LWrappedGraph g x y = LWrappedGraph {
  innerGraph :: g,
  coding     :: Coding y x,
  prettyOuterNode :: y -> String
}

lWrappedGraphI :: (Ord x, Ord y) => LabeledGraphI g x
                                              -> LabeledGraphI (LWrappedGraph g x y) y
lWrappedGraphI innerI = interfaceFromAll dom succ pred hasAr ar prettyNode where
  dom wg = Coding.domain (coding wg)
  succ (LWrappedGraph ig c _) l node = Set.map (decode c) $
                                      successors innerI ig l (encode c node)
  pred (LWrappedGraph ig c _) l node = Set.map (decode c) $
                                      predecessors innerI ig l (encode c node)
  hasAr (LWrappedGraph ig c _) l a = hasArc innerI ig l (encodeArc c a)
  ar (LWrappedGraph ig c _) l = Prelude.map (decodeArc c) $ arcsOfLabel innerI ig l
  encodeArc c (x,y) = (encode c x, encode c y)
  decodeArc c (x,y) = (decode c x, decode c y)
  prettyNode wg n = prettyOuterNode wg n