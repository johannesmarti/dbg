module LiftedGraph (

) where

import Control.Exception.Base (assert)
import Control.Monad.State.Lazy
import Data.Map.Strict as Map
import qualified Data.Set as Set

import CommonLGraphTypes
import LabeledGraph
import Lifted

data Justification x = Base x | Doubleton Int Int

type IntGraph = LMapGraph Int

intGraphI :: LabeledGraphI IntGraph Int
intGraphI = lMapGraphI

data LiftedGraph x = LiftedGraph {
  graph         :: IntGraph,
  justification :: Map Int (Justification x)
}

allJustified :: LiftedGraph x -> Bool
allJustified lg = keysSet (justification lg) == domain intGraphI (graph lg)

justify :: LiftedGraph x -> Int -> Justification x
justify lg node =
  case Map.lookup node (justification lg) of
    Just j  -> j
    Nothing -> error "node is not in LiftedGraph"

topNode :: LiftedGraph x -> Int
topNode lg = case Set.lookupMax (domain intGraphI (graph lg)) of
               Just m  ->  m
               Nothing -> -1

nextNode :: LiftedGraph x -> Int
nextNode lg = topNode lg + 1

combine :: Int -> Int -> State (LiftedGraph x) Int
combine x y = state $ \lg -> let
    newInt = nextNode lg 
    ng = undefined
  in (newInt,ng)
  

