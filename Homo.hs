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
import Graph

type HomoSearch x y = Graph x -> Graph y -> [Function x y]

isHomo :: (Ord x, Ord y) => Function x y -> Graph x -> Graph y -> Bool
isHomo fct d c =
  assert (dom `isSubsetOf` Graph.domain d) $
  assert (Function.range fct `isSubsetOf` Graph.domain c) $
  all pairMapsWell' product where
    dom = Function.domain fct
    product = cartesianProduct labels dom
    pairMapsWell (l,v) = (Set.map (applyFct fct) (successors d l v)) `isSubsetOf`
                                   (successors c l (applyFct fct v))
    pairMapsWell' (l,v) = all succIsWell (successors d l v) where
                            csuccs = successors c l (applyFct fct v)
                            succIsWell s = (applyFct fct s) `elem` csuccs

searchHomos :: (Ord x, Ord y) => HomoSearch x y
searchHomos d c = Prelude.filter (\f -> isHomo f d c)
                                 (allFunctions (Graph.domain d) (Graph.domain c))

noHomo :: HomoSearch x y -> Graph x -> Graph y -> Bool
noHomo search d c = Prelude.null $ search d c
