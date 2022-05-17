module Game (
  game,
  gameEx5,
  gameForce5d,
  gameForce4d,
  gameForce6d,
  gameForce7d,
  gameSlowSquare,
  gameDifficult,
  gameBiggest,
  gameUnsound,
  gameAlsoBig,
  gameTentje,
  gameAlloc1,
  gameAlloc2,
) where

import Control.Monad.State.Lazy

import ConciseGraph
import LiftedGraph
import LiftedGraphReport
import Report
import Patterns

import Label
import Spiral

game = gameAlloc2

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
    lifting = execState combiner (fromLGraph force5dI force5d')
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    --mapM_ (putStrLn . prettyCandidate) cans

gameForce4d :: IO ()
gameForce4d = let
    combiner = do
      combine 0 2
      combine 0 3
      --combine 0 1
      --combine 1 3
      --combine 2 3

      combine 1 5
      combine 4 6

      return ()
    lifting = execState combiner (fromLGraph force4dI force4d')
    ig = graph lifting
    allCans = liftableCandidates ig
    cans = filter (weakDominationFilter ig) allCans
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    easyLiftedGraphReport lifting
    putChar '\n'
    --mapM_ (\c -> putStrLn (prettyCanWithArcs c) >> putChar '\n') cans
    --mapM_ (putStrLn . prettyCandidate) cans
    --putChar '\n'
    print (map extractPair allCans)
    putChar '\n'
    print pairs

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
      combine 1 2
      combine 2 3
      combine 1 3

      combine 0 4
      combine 4 5
      combine 5 6
      combine 3 7
      combine 0 8
      combine 4 9
      combine 8 10
      combine 6 11
      combine 7 12
      combine 6 13
      combine 10 14

      combine 1 2
      combine 2 6
      combine 3 18
      combine 0 19
      combine 4 20
      combine 8 21
      combine 9 22
      combine 10 23
      combine 11 24
      combine 14 25
      combine 15 26
      combine 16 27
      combine 17 28

      combine 16 29
      combine 30 31

      return ()
    lifting = execState combiner (fromLGraph alsoBigI alsoBig)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    easyLiftedGraphReport lifting
    putChar '\n'
    --mapM_ (\c -> putStrLn (prettyCanWithArcs c) >> putChar '\n') cans
    --mapM_ (putStrLn . prettyCandidate) cans
    --putChar '\n'
    --print pairs

gameTentje :: IO ()
gameTentje = let
    combiner = do
      combine 0 3
      combine 1 3
      combine 1 2 -- is forced

      combine 2 3 -- is 7
      combine 0 7
      combine 0 1
      combine 2 4
      combine 4 5
      combine 3 6
      combine 4 9
      combine 1 8
      combine 5 12
      combine 6 11
      combine 2 13
      combine 4 15
      combine 13 14
      combine 5 19 -- is 20
      combine 15 17
      combine 8 21

      combine 0 2
      combine 1 4
      combine 3 23
      combine 4 25
      combine 5 24
      combine 6 27
      combine 7 26
      combine 12 28 -- is 30
      combine 8 29
      combine 9 30
      combine 10 31
      combine 11 32
      combine 16 34
      combine 18 35
      combine 33 36 -- is 37

      return ()
    lifting = execState combiner (fromLGraph (conciseGraphI 4) 3372361817)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    easyLiftedGraphReport lifting
    putChar '\n'
    --mapM_ (\c -> putStrLn (prettyCanWithArcs c) >> putChar '\n') cans
    --mapM_ (putStrLn . prettyCandidate) cans
    --putChar '\n'
    print pairs

gameForce6d :: IO ()
gameForce6d = let
    combiner = do
      combine 2 3
      combine 0 1
      combine 0 2

      return ()
    lifting = execState combiner (fromLGraph force6dI force6d)
    ig = graph lifting
    allCans = liftableCandidates ig
    cans = filter (weakDominationFilter ig) allCans
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    easyLiftedGraphReport lifting
    putChar '\n'
    --easyLiftedGraphReport lifting
    --mapM_ (\c -> putStrLn (prettyCanWithArcs c) >> putChar '\n') cans
    --mapM_ (putStrLn . prettyCandidate) cans
    --putChar '\n'
    print (map extractPair allCans)
    putChar '\n'
    print pairs

gameForce7d :: IO ()
gameForce7d = let
    combiner = do
      combine 2 3
      combine 0 1
      combine 1 2
      combine 3 6
      combine 0 7
      combine 4 8
      combine 5 9

      return ()
    lifting = execState combiner (fromLGraph force7dI force7d)
    ig = graph lifting
    allCans = liftableCandidates ig
    cans = filter (weakDominationFilter ig) allCans
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    easyLiftedGraphReport lifting
    putChar '\n'
    --easyLiftedGraphReport lifting
    --mapM_ (\c -> putStrLn (prettyCanWithArcs c) >> putChar '\n') cans
    --mapM_ (putStrLn . prettyCandidate) cans
    --putChar '\n'
    print (map extractPair allCans)
    putChar '\n'
    print pairs

gameAlloc1 :: IO ()
gameAlloc1 = let
    combiner = do
      combine 0 1
      combine 0 2
      combine 3 4
      return ()
    lifting = execState combiner (fromLGraph alloc1I alloc1)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    --easyLiftedGraphReport lifting
    putChar '\n'
    --mapM_ (\c -> putStrLn (prettyCanWithArcs c) >> putChar '\n') cans
    --mapM_ (putStrLn . prettyCandidate) cans
    --putChar '\n'
    print pairs

gameAlloc2 :: IO ()
gameAlloc2 = let
    combiner = do
      combine 2 3
      combine 0 1
      return ()
    lifting = execState combiner (fromLGraph alloc2I alloc2)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero] [2]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [One] [5]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,One] [5,1]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,Zero,One] [5,4,5]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,One,One] [5,1,5]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,Zero,Zero,One] [5,0,4,5]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,Zero,One,One] [5,4,5,5]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,One,One,One] [5,1,5,5]
    putChar '\n'
    --mapM_ (\c -> putStrLn (prettyCanWithArcs c) >> putChar '\n') cans
    --mapM_ (putStrLn . prettyCandidate) cans
    --putChar '\n'
    print pairs

