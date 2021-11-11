module Bitify (
  bitify,
) where

import qualified Data.Set as Set

import BitGraph
import Coding
import Graph
import WrappedGraph

bitify :: Ord x => GraphI g x -> g -> WrappedGraph BitGraph Node x
bitify gi g = WrappedGraph bitGraphI bg c where
  bg = BitGraph size 0 0
  c = fromAssoc assoc
  oldDom = Graph.domain gi g
  size = Set.size oldDom
  assoc = zip (Set.toList oldDom) [0 .. ]

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

