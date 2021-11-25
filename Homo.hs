module Homo (
  HomoSearch,
  isHomo,
  searchHomos,
  noHomo,
) where

import Control.Exception.Base
import Data.Map.Strict as Map
import Data.Set as Set

import Function
import LabeledGraph

type HomoSearch g1 g2 x y = g1 -> g2 -> [Function x y]

isHomo :: (Ord x, Ord y) => LabeledGraphI g1 x -> LabeledGraphI g2 y -> Function x y -> g1 -> g2 -> Bool
isHomo di ci fct d c =
  assert (dom `isSubsetOf` LabeledGraph.domain di d) $
  assert (Function.range fct `isSubsetOf` LabeledGraph.domain ci c) $
  all pairMapsWell' product where
    dom = Function.domain fct
    product = cartesianProduct labels dom
    pairMapsWell (l,v) = (Set.map (applyFct fct) (successors di d l v)) `isSubsetOf`
                                   (successors ci c l (applyFct fct v))
    pairMapsWell' (l,v) = all succIsWell (successors di d l v) where
                            csuccs = successors ci c l (applyFct fct v)
                            succIsWell s = (applyFct fct s) `elem` csuccs

searchHomos :: (Ord x, Ord y) => LabeledGraphI g1 x -> LabeledGraphI g2 y -> HomoSearch g1 g2 x y
searchHomos di ci d c = Prelude.filter (\f -> isHomo di ci f d c)
                                 (allFunctions (LabeledGraph.domain di d) (LabeledGraph.domain ci c))

noHomo :: HomoSearch g1 g2 x y -> g1 -> g2 -> Bool
noHomo search d c = Prelude.null $ search d c
