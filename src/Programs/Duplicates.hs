module Programs.Duplicates (
  duplicates 
) where

import qualified Data.Set as Set
import System.Environment

import Plans.CoveringGraph

isNubby :: Ord a => [a] -> Maybe a
isNubby list = worker list Set.empty where
  worker [] _ = Nothing
  worker (a:as) seen = if a `Set.member` seen
                         then Just a
                         else worker as (Set.insert a seen)

result :: Int -> String
result numNodes = case isNubby (take numNodes generateNodes) of
  Nothing -> "No duplicates found among the first " ++ show numNodes ++ " elements of the covering graph."
  Just a -> "There is a duplicate at address " ++ show (address a) ++ " in the covering graph."

duplicates :: IO ()
duplicates = do
  args <- getArgs
  let numNodes = read (head args)
  putStrLn (result numNodes)
