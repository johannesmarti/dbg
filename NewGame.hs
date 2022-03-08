module NewGame (
  game,
  gameForce5d,
  gameSlowSquare,
) where

import Control.Monad.State.Lazy

import LiftedGraph
import Patterns

actualGame :: State (LiftedGraph x) [LiftingCandidate]
actualGame = do
  combine 4 7
  combine 3 8
  combine 0 6
  combine 1 12
  combine 10 13
  combine 2 11
  combine 14 15
  lg <- get
  return $ filter (weakDominationFilter (graph lg)) (liftableCandidates (graph lg))

game :: IO ()
game = do
  let (cans,lifting) = runState actualGame (fromLGraph ex5I ex5)
  putStrLn $ unlines $ prettyLiftedGraph lifting
  --mapM_ (putStrLn . prettyCandidate) cans

gameForce5d :: IO ()
gameForce5d = let
    combiner = do
      combine 1 2
      combine 0 2
      combine 0 3
      combine 4 6
      combine 5 7
    lifting = execState combiner (fromLGraph force5dI force5d)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    mapM_ (putStrLn . prettyCandidate) cans

gameSlowSquare :: IO ()
gameSlowSquare = let
    combiner = do
      combine 0 2
      combine 1 2
      combine 0 3
      combine 1 4
      combine 3 4
      combine 0 5
      combine 4 6
      combine 5 7
      combine 6 8
      combine 8 9
      combine 7 13
      combine 8 12
      combine 10 13
      return ()
    lifting = execState combiner (fromLGraph slowSquareI slowSquare)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    mapM_ (putStrLn . prettyCandidate) cans
