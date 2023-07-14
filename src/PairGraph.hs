module PairGraph (
  PairGraph,
  zeroGraph, oneGraph,
  pairGraphInterface,
  graphOfLabel,
  fromFunction,
) where

import Control.Exception.Base
import qualified Data.Set as Set

import qualified GraphInterface as GI
import LabeledGraphInterface

data PairGraph g = PairGraph {
  zeroGraph :: g,
  oneGraph  :: g
}

instance Functor PairGraph where
  fmap f (PairGraph zg og) = PairGraph (f zg) (f og)

pairGraphInterface :: Eq x => GI.GraphInterface g x -> LabeledGraphInterface(PairGraph g) x
pairGraphInterface innerI = iFromAll dom succ pred hasAr ar pretty where
  dom pg = let zd = GI.domain innerI (zeroGraph pg)
               od = GI.domain innerI (oneGraph pg)
           in assert (zd == od) zd
  succ pg label = GI.successors innerI (graphOfLabel pg label)
  pred pg label = GI.predecessors innerI (graphOfLabel pg label)
  hasAr pg label = GI.hasArc innerI (graphOfLabel pg label)
  ar pg l = GI.arcs innerI (graphOfLabel pg l)
  pretty pg = GI.prettyNode innerI (zeroGraph pg)

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

