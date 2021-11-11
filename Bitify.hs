module Bitify (

) where

import BitGraph
import Coding
import Graph
import WrappedGraph

bitify :: Ord x => GraphI g x -> g -> WrappedGraph BitGraph x Node
bitify gi g = undefined

{-
-- Should Maybe stay on top of sets?
linearization :: Ord a => ParityGame a -> Linearization a
linearization game = Linearization 
  linGame
  encoder
  decoder where
    linGame = linearFromGraph smallerLessImportant
                (applyIso (fromJust . encoder) graph)
                ((toMove game) . fromJust . decoder)
    graph = gameGraph game
    dom = Set.toList (domain graph)
    --graph = graphOnDomain dom priority
    (noIters,iters) = partition (isNothing . (canIterate game)) dom
    unranked = zip noIters [0..]
    itersOrderedByIncreasingPriority = sortBy (lessImportantFirst (ranking game)) iters
    liftedPriorities = liftPrioritiesTo (winsIteration game) (length noIters) itersOrderedByIncreasingPriority
    reAssoc = unranked ++ liftedPriorities
    newDom = map snd reAssoc
    enMap = Map.fromList reAssoc
    deMap = Map.fromList (map swap reAssoc)
    encoder obj = Map.lookup obj enMap
    decoder n = Map.lookup n deMap
-}

