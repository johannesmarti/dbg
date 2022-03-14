module Game (
  game,
  gameEx5,
  gameForce5d,
  gameSlowSquare,
  gameDifficult,
  gameBiggest,
  gameUnsound,
  gameAlsoBig,
) where

import Control.Monad.State.Lazy

import LiftedGraph
import LiftedGraphReport
import Report
import Patterns

game = gameEx3

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

      combine 2 4 -- gives 8 for 101
      combine 1 5 -- gives 9 for 011
      combine 4 9 -- gives 10 for 110
      combine 1 4 -- gives 11 for 010
      combine 4 5 -- gives 12 for 100
      combine 6 12 -- gives 13 for 001

      combine 0 1 -- gives 14 for 10
      combine 2 3 -- gives 15 for 01
      combine 2 14 -- gives 16 for 10
      combine 1 15 -- gives 17 for 01
      combine 4 16 -- gives 18 for 10
      combine 5 17 -- gives 19 for 01
      combine 6 18 -- gives 20 for 10
      combine 7 19 -- gives 21 for 01
      combine 5 20 -- gives 22 for 10
      combine 6 21 -- gives 23 for 01
      combine 8 22 -- gives 24 for 10
      combine 9 23 -- gives 25 for 01
      combine 13 24 -- gives 26 for 10 and 1
    
      combine 10 13 -- gives 27 for 0
      combine 13 27 -- gives 28 for 0
      combine 25 28 -- gives 29 for 0
      combine 26 29 -- gives the double self loop!!!!
      return ()
    lifting = execState combiner (fromLGraph biggestI biggest)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    easyWordReport 15 intGraphI ig
    --mapM_ (putStrLn . prettyCandidate) cans
    putChar '\n'
    print pairs

gameUnsound :: IO ()
gameUnsound = let
    combiner = do
      combine 0 3 -- gives 4 for 01
      combine 1 3 -- gives 5 for 0
      combine 0 2 -- gives 6 for 1
      combine 0 5 -- gives 7 for 0
      combine 2 7 -- gives 8 for 0
      combine 4 8 -- gives 9 for 0
      combine 6 9 -- gives double self-loop
      return ()
    lifting = execState combiner (fromLGraph unsoundI unsound)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    easyWordReport 15 intGraphI ig
    --mapM_ (putStrLn . prettyCandidate) cans
    putChar '\n'
    print pairs

gameEx3 :: IO ()
gameEx3 = let
    combiner = do
      combine 1 4
      combine 0 3
      combine 2 6
      combine 5 7
      return ()
    lifting = execState combiner (fromLGraph ex3I ex3)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    easyWordReport 15 intGraphI ig
    --mapM_ (putStrLn . prettyCandidate) cans
    putChar '\n'
    print pairs

gameAlsoBig :: IO ()
gameAlsoBig = let
    combiner = do
      return ()
    lifting = execState combiner (fromLGraph alsoBigI alsoBig)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    easyLiftedGraphReport lifting
    --mapM_ (putStrLn . prettyCandidate) cans
    putChar '\n'
    print pairs
