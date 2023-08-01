module Plans.Spoke (
  Spoke,
  spoke,
  singleton,
  hub,
  pointsAList,
  distanceOf,
  Plans.Spoke.nodes,
  singletonNode,
  Plans.Spoke.contained,
  Plans.Spoke.insert,
  pointsAtDistanceList,
  maximalDistance,
) where

import Control.Exception (assert)
import qualified Data.Set as S
import Data.Tuple (swap)

--import Plans.AListDistanceData as DistanceData
import Plans.MapDistanceData as DistanceData

data Spoke x = Spoke {
  hub    :: x,
  points :: DistanceData x
} deriving Show

instance Ord x => Eq (Spoke x) where
  s == s' = points s `DistanceData.equalMap` points s'

isCoherent :: Ord x => Spoke x -> Bool
isCoherent s = DistanceData.isCoherent (points s)
                 && pointsAtDistanceList 0 s == [hub s]

plainSpoke :: Ord x => x -> DistanceData x -> Spoke x
plainSpoke h p = assert (Plans.Spoke.isCoherent result) result where
  result = Spoke h p

spoke :: Ord x => x -> [(x,Int)] -> Spoke x
spoke h p = plainSpoke h (DistanceData.fromAList ((h,0) : p))

-- This does unfortunately not use plainSpoke because we don't want to depend
-- on Eq
singleton :: x -> Spoke x
singleton a = Spoke a (DistanceData.hubSingleton a)

insert :: Ord x => x -> Int -> Spoke x -> Spoke x
insert key value (Spoke h ps) = plainSpoke h (DistanceData.insert key value ps)

nodes :: Ord x => Spoke x -> S.Set x
nodes = DistanceData.nodes . points

pointsAList :: Spoke x -> [(x,Int)]
pointsAList = DistanceData.toAList . points

distanceOf :: Ord x => Spoke x -> x -> Int
distanceOf sp node = case DistanceData.lookup node (points sp) of
                       Just d  -> d
                       Nothing -> error "node is not in spiral"

inverse :: [(x,y)] -> [(y,x)]
inverse = map swap

lookupAll :: Eq x => x -> [(x,y)] -> [y]
lookupAll key pairList = [v | (k,v) <- pairList, k == key]

pointsAtDistanceList :: Int -> Spoke x -> [x]
pointsAtDistanceList distance = lookupAll distance . inverse . pointsAList

maximalDistance :: Spoke x -> Int
maximalDistance = maximum . map snd . pointsAList

{-
isSingleton :: Ord x => Spoke x -> Bool
isSingleton = isJust . singletonNode
-}

singletonNode :: Spoke x -> Maybe x
singletonNode s = case pointsAList s of
                    [(n,_)] -> Just n
                    _       -> Nothing

contained :: Ord x => x -> Spoke x -> Bool
contained x s = x `DistanceData.contained` points s

