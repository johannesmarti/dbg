module Game (
  game,
  gameEx5,
  gameForce5d,
  gameForce4d,
  gameForce6d,
  gameForce7d,
  gameForce9d,
  gameSlowSquare,
  gameDifficult,
  gameBiggest,
  gameUnsound,
  gameAlsoBig,
  gameTentje,
  gameAlloc1,
  gameAlloc2,
  gameAlloc3,
  gameIssues,
  gameDbg3,
  gameDbg4,
  gameDbg5,
) where

import Control.Monad.State.Lazy

import ConciseGraph
import DeBruijnGraph
import LiftedGraph
import LiftedGraphReport
import Report
import Patterns
import Bitable

import Label
import Spiral

game = gameStudy

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
      -- end of forced

      combine 0 3 -- seems best for 101
      combine 0 1 -- clearly best (creates 7)
      combine 1 3 -- what a smart move. Creates someone that is easy to see

      -- wind up 01  with 8,7
      combine 3 7 -- gives 9 for 10
      combine 0 8 -- gives 10 for 01
      combine 4 9 -- gives 11 for 10
      combine 5 10 -- gives 12 for 01
      combine 6 11 -- gives 13 for 10
      combine 12 13
      return ()
    lifting = execState combiner (fromLGraph slowSquareI slowSquare)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    print pairs

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
      combine 8 22 -- gives 23 for 10
      combine 9 21 -- gives 24 for 01
      combine 13 23 -- gives 25 for 10 and 1
    
      combine 9 22 -- gives 26 for 0
      combine 24 26 -- gives 27 for 0
      combine 25 27 -- gives double self loop!!
      return ()
    lifting = execState combiner (fromLGraph biggestI biggest)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
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
    --easyWordReport 15 intGraphI ig
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

      -- winding up 01
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

      combine 29 30

      return ()
    lifting = execState combiner (fromLGraph alsoBigI alsoBig)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
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
      combine 1 2
      combine 0 6
      combine 4 7
      combine 5 8
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
    print $ Spiral.fromHub intGraphI ig [One] [1]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,One] [0,1]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,Zero,One] [0,2,1]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,One,One] [0,1,1]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,Zero,Zero,One] [1,0,2,1]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,Zero,One,One] [0,2,1,1]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,One,One,One] [0,1,1,1]
    putChar '\n'
    --mapM_ (\c -> putStrLn (prettyCanWithArcs c) >> putChar '\n') cans
    --mapM_ (putStrLn . prettyCandidate) cans
    --putChar '\n'
    print pairs

gameAlloc3 :: IO ()
gameAlloc3 = let
    combiner = do
      combine 2 3 -- 4 for 01
      combine 0 1 -- 5 for 1
      combine 0 3 -- 6 for 0000
      combine 2 6 -- 7 for 000
      combine 1 7 -- 8 for 00
      combine 4 8 -- 9 for 0
      combine 5 9 
      return ()
    lifting = execState combiner (fromLGraph alloc3I alloc3)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    print pairs

gameIssues :: IO ()
gameIssues = let
    combiner = do
      combine 1 4
      combine 3 5
      combine 0 2
      combine 6 7
      return ()
    lifting = execState combiner (fromLGraph issuesI issues)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    easyLiftedGraphRelReport lifting [One]
    putChar '\n'
    easyLiftedGraphRelReport lifting [Zero,One]
    putChar '\n'
    easyLiftedGraphRelReport lifting [Zero,Zero,One]
    putChar '\n'
    easyLiftedGraphRelReport lifting [Zero,One,One]
    putChar '\n'
    print pairs

gameDbg3 :: IO ()
gameDbg3 = let
    combiner = do
      -- create 01 universal
      combine 0 1 -- 8
      -- create 10 universal
      combine 6 7 -- 9

      -- wrap up 01
      combine 2 3 -- 10
      combine 4 5 -- 11
      combine 8 10 -- 12
      combine 9 11 -- 13

      combine 12 13
      return ()
    lifting = execState combiner (fromLGraph dbgI (dbg 3))
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    easyLiftedGraphRelReport lifting [Zero]
    putChar '\n'
    --easyLiftedGraphRelReport lifting [Zero,One]
    --putChar '\n'
    --print $ Spiral.fromHub intGraphI ig [Zero,One] [9,5]
    --putChar '\n'
    --easyLiftedGraphRelReport lifting [Zero,Zero,One]
    --putChar '\n'
    --easyLiftedGraphRelReport lifting [Zero,One,One]
    --putChar '\n'
    print pairs

gameDbg4 :: IO ()
gameDbg4 = let
    combiner = do
      combine 2 3 -- 16 create 01 universal
      combine 12 13 -- 17 create 10 universal

      -- wrap up 01
      combine 4 5 -- 18 -> 01
      combine 10 11 -- 19 -> 10
      combine 7 18 -- 20 -> 01
      combine 8 19 -- 21 -> 10
      combine 6 20 -- 22 -> 01
      combine 9 21 -- 23 -> 10

      -- wrap up 0
      combine 0 1 -- 24
      combine 16 24 -- 25
      combine 22 25 -- 26

      -- wrap up 1
      combine 14 15 -- 27
      combine 17 27 -- 28
      combine 23 28 -- 29

      combine 26 29
      return ()
    lifting = execState combiner (fromLGraph dbgI (dbg 4))
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    print pairs

gameDbg5 :: IO ()
gameDbg5 = let
    combiner = do
      -- wrap up 0001
      a0001 <- combine 2 3 -- 32

      -- wrap up 1110
      a1110 <- combine 28 29 -- 33

      -- wrap up 001
      a001 <- combine 4 5 -- 34 at 001
      a100 <- combine 18 19 -- 35 at 100
      b001 <- combine 6 a001 -- 36 at 001
      c001 <- combine 7 b001 -- 37 at 001

      -- wrap up 110
      a110 <- combine 26 27 -- 38 at 110
      a011 <- combine 12 13 -- 39 at 011
      b110 <- combine 25 a110 -- 40 at 110
      c110 <- combine 24 b110 -- 41 at 110

      -- wrap up 01
      a01 <- combine 10 11 -- 42 at 01
      a10 <- combine 20 21 -- 43 at 10
      b01 <- combine 9 a01 -- 44 at 01
      b10 <- combine 22 a10 -- 45 at 10
      c01 <- combine 8 b01 -- 46 at 01
      c10 <- combine 23 b10 -- 47 at 10
      d01 <- combine a011 c01 -- 48 at 01
      d10 <- combine a100 c10 -- 49 at 10
      e01 <- combine 14 d01 -- 50 at 01
      f01 <- combine 15 e01 -- 51 at 01
      e10 <- combine 16 d10 -- 52 at 10
      f10 <- combine 17 e10 -- 53 at 10

      -- wrap up 0
      a0 <- combine 0 1 -- 54 at 0
      b0 <- combine a0001 a0 -- 55 at 0
      c0 <- combine c001 b0 -- 56 at 0
      d0 <- combine f01 c0 -- 57 at 0

      -- wrap up 1
      a1 <- combine 30 31 -- 58 at 1
      b1 <- combine a1110 a1 -- 59 at 1
      c1 <- combine c110 b1 -- 60 at 1
      d1 <- combine f10 c1 -- 61 at 1

      combine d0 d1

      return ()
    lifting = execState combiner (fromLGraph dbgI (dbg 5))
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    print pairs


gameForce9d :: IO ()
gameForce9d = let
    combiner = do
      combine 0 2 -- 4
      combine 2 3 -- 5
      combine 0 1 -- 6 for 1
      combine 1 2 -- 7
      combine 0 7 -- 8
      combine 3 8 -- 9
      combine 4 9 -- 10
      combine 5 10 -- 11
      combine 6 11 -- done
      return ()
    lifting = execState combiner (fromLGraph force9dI force9d)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    print pairs

gameStudy :: IO ()
gameStudy = let
    combiner = do
      -- wrap up 0001
      a0001 <- combine 2 3 -- 32

      -- wrap up 1110
      a1110 <- combine 28 29 -- 33

      -- wrap up 001
      a001 <- combine 4 5 -- 34 at 001
      a100 <- combine 18 19 -- 35 at 100
      b001 <- combine 6 a001 -- 36 at 001
      c001 <- combine 7 b001 -- 37 at 001

      -- wrap up 001
      a110 <- combine 26 27 -- 38 at 110
      a011 <- combine 12 13 -- 39 at 011
      b110 <- combine 25 a110 -- 40 at 110
      c110 <- combine 24 b110 -- 41 at 110

      -- wrap up 01
      a01 <- combine 10 11 -- 42 at 01
      a10 <- combine 20 21 -- 43 at 10
      b01 <- combine 9 a01 -- 44 at 01
      b10 <- combine 22 a10 -- 45 at 10
      c01 <- combine 8 b01 -- 46 at 01
      c10 <- combine 23 b10 -- 47 at 10
      d01 <- combine a011 c01 -- 48 at 01
      d10 <- combine a100 c10 -- 49 at 10
      e01 <- combine 14 d01 -- 50 at 01
      f01 <- combine 15 e01 -- 51 at 01
      e10 <- combine 16 d10 -- 52 at 10
      f10 <- combine 17 e10 -- 53 at 10

      -- wrap up 0
      a0 <- combine 0 1 -- 54 at 0
      b0 <- combine a0001 a0 -- 55 at 0
      c0 <- combine c001 b0 -- 56 at 0
      d0 <- combine f01 c0 -- 57 at 0

      -- wrap up 1
      a1 <- combine 30 31 -- 58 at 1
      b1 <- combine a1110 a1 -- 59 at 1
      c1 <- combine c110 b1 -- 60 at 1
      d1 <- combine f10 c1 -- 61 at 1

      combine d0 d1

      return ()
    lifting = execState combiner (fromLGraph dbgI (dbg 6))
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    --easyLiftedGraphRelReport lifting [Zero,One,One]
    --putChar '\n'
    --easyWordReport 15 intGraphI ig
    --putChar '\n'
    putChar '\n'
    print pairs
    --mapM_ (putStrLn . prettyCandidate) cans

