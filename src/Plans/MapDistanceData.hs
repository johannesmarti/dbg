module Plans.MapDistanceData (
  DistanceData,
  equalMap,
  isCoherent,
  fromAList,
  toAList,
  hubSingleton,
  insert,
  nodes,
  contained,
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type DistanceData x = M.Map x Int

equalMap :: Ord x => DistanceData x -> DistanceData x -> Bool
equalMap d d' = d == d'

isCoherent :: DistanceData x -> Bool
isCoherent ps = True

fromAList :: Ord x => [(x,Int)] -> DistanceData x
fromAList = M.fromList

toAList :: DistanceData x -> [(x,Int)]
toAList = M.toList

-- This does unfortunately not use plainSpoke because we don't want to depend
-- on Eq
hubSingleton :: x -> DistanceData x
hubSingleton a = M.singleton a 0

insert :: Ord x => x -> Int -> DistanceData x -> DistanceData x
insert = M.insert

nodes :: DistanceData x -> S.Set x
nodes = M.keysSet

contained :: Ord x => x -> DistanceData x -> Bool
contained = M.member
