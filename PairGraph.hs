module PairGraph (
  PairGraph,
  zeroGraph, oneGraph,
  pairGraphI,
  graphOfLabel,
  fromFunction,
) where

import Control.Exception.Base
import qualified Data.Set as Set

import qualified Graph
import LabeledGraph

data PairGraph g = PairGraph {
  zeroGraph :: g,
  oneGraph  :: g
}

instance Functor PairGraph where
  fmap f (PairGraph zg og) = PairGraph (f zg) (f og)

pairGraphI :: Eq x => Graph.GraphI g x -> LabeledGraphI(PairGraph g) x
pairGraphI innerI = interfaceFromAll dom succ pred ar pretty where
  dom pg = let zd = Graph.domain innerI (zeroGraph pg)
               od = Graph.domain innerI (oneGraph pg)
           in assert (zd == od) zd
  succ pg label = Graph.successors innerI (graphOfLabel pg label)
  pred pg label = Graph.predecessors innerI (graphOfLabel pg label)
  ar pg = let ins label = map (i label)
              i label (x,y) = (x,label,y)
          in (ins Zero $ Graph.arcs innerI (zeroGraph pg))
             ++ (ins One $ Graph.arcs innerI (oneGraph pg))
  pretty pg = Graph.prettyNode innerI (zeroGraph pg)

graphOfLabel :: PairGraph g -> Label -> g
graphOfLabel pg Zero = zeroGraph pg
graphOfLabel pg One  = oneGraph pg

fromFunction :: (Label -> g) -> PairGraph g
fromFunction fct = PairGraph (fct Zero) (fct One)

{-
fromArcs :: Size -> [Arc Node] -> BitGraph
fromArcs size arcs = BitGraph size zbm obm where
  (zbm, obm) = foldl setLabeledArc (nullWord, nullWord) arcs
  setLabeledArc (zeroWord,oneWord) (u,Zero,v) = (setArc size zeroWord (u,v), oneWord)
  setLabeledArc (zeroWord,oneWord) (u,One ,v) = (zeroWord, setArc size oneWord (u,v))
-}

