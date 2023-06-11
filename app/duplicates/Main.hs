module Main (
  main
) where

import qualified Data.Set as Set
import System.Environment

import CoveringGraph
import Label
import Word

a :: [Label]
a = [Zero,One,One,One]
b = a ++ [Zero,One,Zero,Zero,Zero] ++ a ++ [Zero,One,One]

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

main :: IO ()
main = do
  args <- getArgs
  let numNodes = read (head args)
  putStrLn (result numNodes)
