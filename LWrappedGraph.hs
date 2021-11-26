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
  coding     :: Coding y x
}

lWrappedGraphI :: (Ord x, Ord y, Pretty y) => LabeledGraphI g x
                                              -> LabeledGraphI (LWrappedGraph g x y) y
lWrappedGraphI innerI = interfaceFromAll dom succ pred ar prettyNode where
  dom wg = Coding.domain (coding wg)
  succ (LWrappedGraph ig c) l node = Set.map (decode c) $
                                      successors innerI ig l (encode c node)
  pred (LWrappedGraph ig c) l node = Set.map (decode c) $
                                      predecessors innerI ig l (encode c node)
  ar (LWrappedGraph ig c) l = Prelude.map (decodeArc c) $ arcsOfLabel innerI ig l
  decodeArc c (x,y) = (decode c x, decode c y)
  prettyNode _ n = pretty n
