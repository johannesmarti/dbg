module ConstructionGraph (

) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import LabeledGraph
import CommonLGraphTypes
import PairGraph
import qualified FunctionGraph as FG
import Tools (strictPairs)

constructionGraph :: Ord x => LabeledGraphI g x -> g
                              -> (LMapGraph (x,x), Map.Map (x,x) Int)
constructionGraph gi g = let
    dom = Set.fromList . strictPairs . Set.toList $ domain gi g
    succ label = undefined
    pred label = undefined
    component label = FG.fromDomSuccPred dom (succ label) (pred label)
    fctGraph = PairGraph.fromFunction component
    cGraph = undefined
  in (cGraph, undefined)
