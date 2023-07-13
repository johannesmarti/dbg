{-# LANGUAGE FlexibleContexts #-} -- This is enabled because of issues with the State monad below
module Plan (
  Spoke,
  hub,
  spoke,
  pointsAtDistance,
  maximalDistance,
  isSingleton,
  singletonNode,
  contained,
  Plan,
  Plan.empty,
  Plan.insert,
) where

-- TODO: Should we use the strict or the lazy state monad? Need to read up!
import qualified Data.Map.Strict as M

import WordMap.Algebraic as WordMap
import Label

{-
 We have a WordTree that maps addresses of Covering nodes to a pointed set. The
point denotes the center of the spiral at the turningWord of the Covering node
and the remaining elements in the set are the required nodes for the covering.
-}

-- TODO: This data type should be used or unified with the implementation of Spiral
data Spoke x = Spoke {
  hub    :: x,
  points :: M.Map x Int
}

spoke :: Ord x => x -> [(x,Int)] -> Spoke x
spoke h p = Spoke h (M.insert h 0 (M.fromList p))

pointsAtDistance :: Int -> Spoke x -> [x]
pointsAtDistance distance =
  map fst . filter (\(p,d) -> d == distance) . M.toList . points

maximalDistance :: Spoke x -> Int
maximalDistance = maximum . M.elems . points

isSingleton :: Spoke x -> Bool
isSingleton s = M.size (points s) <= 1

singletonNode :: Spoke x -> Maybe x
singletonNode s = if isSingleton s
                        then Just (hub s)
                        else Nothing

contained :: Ord x => x -> Spoke x -> Bool
contained x (Spoke _ m) = x `M.member` m

type Plan x = WordMap (Spoke x)

empty :: Plan x
empty = WordMap.empty

insert :: [Label] -> Spoke x -> Plan x -> Plan x
insert = WordMap.insert