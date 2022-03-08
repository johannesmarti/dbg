module Game (
  gameEx5,
  gameForce5d,
  gameSlowSquare,
) where

import Control.Monad.State.Lazy

import LiftedGraph
import Report
import Patterns

gameEx5 :: IO ()
gameEx5 = let
    combiner = do
      combine 4 7
      combine 3 8
      combine 0 6
      combine 1 12
      combine 10 13
      combine 2 11
      combine 14 15
    lifting = execState combiner (fromLGraph ex5I ex5)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
  in do
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
      combine 0 1
      combine 1 3
      s' <- combine 3 7
      e' <- combine 4 8
      s'' <- combine 4 s'
      e'' <- combine 5 e'
      s''' <- combine 6 s''
      combine 7 12
      combine 13 14
      return ()
    lifting = execState combiner (fromLGraph slowSquareI slowSquare)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    easyWordReport 15 intGraphI ig
    mapM_ (putStrLn . prettyCandidate) cans
