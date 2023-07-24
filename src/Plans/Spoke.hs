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

-- TODO: Should we use the strict or the lazy state monad? Need to read up!
import qualified Data.Map.Strict as M

data Spoke x = Spoke {
  hub    :: x,
  points :: M.Map x Int
}

spoke :: Ord x => x -> [(x,Int)] -> Spoke x
spoke h p = Spoke h (M.insert h 0 (M.fromList p))

pointsAtDistance :: Int -> Spoke x -> [x]
pointsAtDistance distance =
  map fst . filter (\(_,d) -> d == distance) . M.toList . points

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

