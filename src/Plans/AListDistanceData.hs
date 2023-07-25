module Plans.AListDistanceData (
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

import Data.List (sortBy)
import qualified Data.Set as S

type DistanceData x = [(x,Int)]

equalMap :: Ord x => DistanceData x -> DistanceData x -> Bool
equalMap d d' = let sortWith f = sortBy (\a b -> f a `compare` f b)
                    sorted = sortWith fst
                   in sorted d == sorted d'

lookupAll :: Eq x => x -> [(x,y)] -> [y]
lookupAll key pairList = [v | (k,v) <- pairList, k == key]

isNub :: Eq x => [(x,y)] -> Bool
isNub [] = True
isNub ((a,_):rest) = null (lookupAll a rest) && isNub rest

isCoherent :: Eq x => DistanceData x -> Bool
isCoherent ps = isNub ps

fromAList :: [(x,Int)] -> DistanceData x
fromAList = id

toAList :: DistanceData x -> [(x,Int)]
toAList = id

-- This does unfortunately not use plainSpoke because we don't want to depend
-- on Eq
hubSingleton :: x -> DistanceData x
hubSingleton a = [(a,0)]

insert :: x -> Int -> DistanceData x -> DistanceData x
insert key value alist = ((key,value) : alist)

nodes :: Ord x => DistanceData x -> S.Set x
nodes = S.fromList . map fst

contained :: Eq x => x -> DistanceData x -> Bool
contained a alist = a `elem` (map fst alist)
