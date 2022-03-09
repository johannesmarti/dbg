module LiftingSearch (
  searchLifting,
) where

import Control.Monad.Plus (msum)
import Control.Monad.State.Lazy

import LiftedGraph
import LabeledGraph

searchLifting :: Ord x => LabeledGraphI g x -> g -> Int -> Maybe (LiftedGraph x)
searchLifting gi g boundOnNumNodes = let
    lg = fromLGraph gi g
  in if hasDoubleRefl gi g
       then Just lg
       else realSearch lg boundOnNumNodes

realSearch :: LiftedGraph x -> Int -> Maybe (LiftedGraph x)
realSearch lg bound = if bound <= 0 then Nothing else let
    ig = graph lg
    candidates = filter (dominationFilter ig) (liftableCandidates ig)
    dealWithCandidate can = let
        (newPoint, nextGraph) = runState (liftCandidate can) lg
      in if hasBothLoops intGraphI (graph nextGraph) newPoint
           then Just nextGraph
           else realSearch nextGraph (bound - 1)
  in msum (map dealWithCandidate candidates)
