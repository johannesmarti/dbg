module Plans.Spoke (
  Spoke,
  hub,
  spoke,
  pointsAtDistance,
  maximalDistance,
  isSingleton,
  singletonNode,
  contained,
) where

import Control.Exception (assert)
import Data.Maybe (isJust)
import Data.Tuple (swap)

data Spoke x = Spoke {
  hub    :: x,
  points :: [(x,Int)]
}

isNub :: Eq x => [(x,y)] -> Bool
isNub [] = True
isNub ((a,_):rest) = null (lookupAll a rest) && isNub rest

isCoherent :: Eq x => Spoke x -> Bool
isCoherent (Spoke h ps) =
  isNub ps &&
  (lookupAll 0 . inverse $ ps) == [h]

spoke :: Eq x => x -> [(x,Int)] -> Spoke x
spoke h p = plainSpoke h ((h,0) : p)

plainSpoke :: Eq x => x -> [(x,Int)] -> Spoke x
plainSpoke h p = assert (isCoherent result) result where
  result = Spoke h p

insert :: Eq x => x -> Int -> Spoke x -> Spoke x
insert key value (Spoke h ps) = plainSpoke h ((key,value) : ps)

inverse :: [(x,y)] -> [(y,x)]
inverse = map swap

lookupAll :: Eq x => x -> [(x,y)] -> [y]
lookupAll key list = [b | (a,b) <- list, a == key]

nodes :: Spoke x -> [x]
nodes = map fst . points

pointsAtDistance :: Int -> Spoke x -> [x]
pointsAtDistance distance = lookupAll distance . inverse . points

maximalDistance :: Spoke x -> Int
maximalDistance = maximum . map snd . points

isSingleton :: Spoke x -> Bool
isSingleton = isJust . singletonNode

singletonNode :: Spoke x -> Maybe x
singletonNode s = case points s of
                    [(n,d)]   -> Just n
                    otherwise -> Nothing

contained :: Ord x => x -> Spoke x -> Bool
contained x s = x `elem` nodes s

