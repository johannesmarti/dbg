module Lifting (
  lifting,
) where

import qualified Data.Set as Set

import AssocGraph
import Graph

lifting :: Ord x => GraphI g x -> g -> AssocGraph (x,x)
lifting gi g l = edges where
  oldDom = domain gi g
  allPairsOfOld = Set.cartesianProduct oldDom oldDom
  dom = Set.filter (\(a,b) -> a <= b) allPairsOfOld
  allPairs = Set.toList (Set.cartesianProduct dom dom)
  edges = filter sees allPairs
  sees ((v,v'),(u,u')) = let sucv  = successors gi g l v
                             sucv' = successors gi g l v'
                           in (u `Set.member` sucv  && u' `Set.member` sucv) ||
                              (u `Set.member` sucv' && u' `Set.member` sucv')
