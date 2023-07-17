module HomomorphismSearch.Homomorphism (
  HomomorphismSearch,
  isHomomorphism,
  searchHomomorphisms,
  noHomomorphism,
) where

import Control.Exception.Base
import Data.Set as Set

import Data.FiniteFunction as Function
import Data.Label
import Graphs.LabeledGraphInterface as LGI

type HomomorphismSearch g1 g2 x y = g1 -> g2 -> [Function x y]

isHomomorphism :: (Ord x, Ord y) => LabeledGraphInterface g1 x -> LabeledGraphInterface g2 y -> Function x y -> g1 -> g2 -> Bool
isHomomorphism di ci fct d c =
  assert (dom `isSubsetOf` LGI.domain di d) $
  assert (Function.range fct `isSubsetOf` LGI.domain ci c) $
  all pairMapsWell' product where
    dom = Function.domain fct
    product = cartesianProduct labels dom
    pairMapsWell (l,v) = (Set.map (applyFct fct) (successors di d l v)) `isSubsetOf`
                                   (successors ci c l (applyFct fct v))
    pairMapsWell' (l,v) = all succIsWell (successors di d l v) where
                            csuccs = successors ci c l (applyFct fct v)
                            succIsWell s = (applyFct fct s) `elem` csuccs

searchHomomorphisms :: (Ord x, Ord y) => LabeledGraphInterface g1 x -> LabeledGraphInterface g2 y -> HomomorphismSearch g1 g2 x y
searchHomomorphisms di ci d c = Prelude.filter (\f -> isHomomorphism di ci f d c)
                                 (allFunctions (LGI.domain di d) (LGI.domain ci c))

noHomomorphism :: HomomorphismSearch g1 g2 x y -> g1 -> g2 -> Bool
noHomomorphism search d c = Prelude.null $ search d c
