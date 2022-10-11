module LiftingSearch (
  searchLifting,
) where

--import Control.Monad.Plus (msum)
import Control.Monad.State.Lazy

import LiftedGraph
import LabeledGraph

searchLifting :: Ord x => Int -> LabeledGraphI g x -> g -> Maybe (LiftedGraph x)
searchLifting boundOnNumNodes gi g = let
    lg = fromLGraph gi g
  in if hasDoubleRefl gi g
       then Just lg
       else realSearch boundOnNumNodes lg

realSearch :: Int -> LiftedGraph x -> Maybe (LiftedGraph x)
realSearch bound lg = if bound <= 0 then Nothing else let
    ig = graph lg
    candidates = filter (dominationFilter ig) (liftableCandidates ig)
    dealWithCandidate can = let
        (newPoint, nextGraph) = runState (liftCandidate can) lg
      in if hasBothLoops intGraphI (graph nextGraph) newPoint
           then Just nextGraph
           else realSearch (bound - 1) nextGraph
  in msum (map dealWithCandidate candidates)
