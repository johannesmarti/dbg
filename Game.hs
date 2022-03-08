module Game (
  game,
  gameEx5,
  gameForce5d,
  gameSlowSquare,
  gameDifficult,
  gameBiggest,
) where

import Control.Monad.State.Lazy

import LiftedGraph
import Report
import Patterns

game = gameBiggest

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

gameDifficult :: IO ()
gameDifficult = let
    combiner = do
      combine 0 3
      combine 1 3
      combine 0 2
      combine 2 3
      combine 0 7
      combine 1 8
      combine 4 9
      combine 5 10
      combine 6 11
      return ()
    lifting = execState combiner (fromLGraph difficultI difficult)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    --easyWordReport 15 intGraphI ig
    --mapM_ (putStrLn . prettyCandidate) cans

gameBiggest :: IO ()
gameBiggest = let
    combiner = do
      combine 0 3
      combine 0 2
      combine 1 2
      combine 1 3
      combine 2 4
      combine 0 1    -- is  9
      combine 2 3    -- is 10
      combine 2 9    -- is 11
      combine 1 10   -- is 12
      combine 4 11   -- is 13
      combine 10 12  -- is 14
      combine 4 14   -- is 15
      combine 5 13   -- is 16
      combine 6 15   -- is 17
      combine 4 6
      combine 5 7
      combine 6 8
      combine 7 16
      return ()
    lifting = execState combiner (fromLGraph biggestI biggest)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = liftablePairs ig
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    easyWordReport 15 intGraphI ig
    --mapM_ (putStrLn . prettyCandidate) cans
    putChar '\n'
    print pairs
