module Plans.Plan (
  Spoke,
  hub,
  spoke,
  pointsAtDistance,
  maximalDistance,
  isSingleton,
  singletonNode,
  contained,
  Plan,
  Plans.Plan.empty,
  Plans.Plan.insert,
) where

import Data.WordMaps.Algebraic as WordMap
import Data.Label
import Plans.Spoke
import Plans.CoveringGraph

-- TODO: This data type should be used or unified with the implementation of Spiral
type Plan x = WordMap (Spoke x)

empty :: Plan x
empty = WordMap.empty

insert :: [Label] -> Spoke x -> Plan x -> Plan x
insert = WordMap.insert
