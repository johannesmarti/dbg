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
  gameBig5,
  gameIssues,
  gameDbg3,
  gameDbg4,
) where

import Control.Monad.State.Lazy

import ConciseGraph
import DeBruijnGraph
import LiftedGraph
import LiftedGraphReport
import Report
import Patterns

import Label
import Spiral

game = gameStudyBiggest

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

gameBig5 :: IO ()
gameBig5 = let
    combiner = do
      combine 2 3

      -- to get 0 2
      combine 1 3
      combine 0 1
      combine 0 2 -- 8 for 10

      combine 3 8 -- 9 for 10

      -- to get 4 9
      combine 3 4
      combine 1 4
      combine 0 4
      combine 2 4
      combine 4 5
      combine 4 6
      combine 4 7
      combine 4 9 -- 17 for 10

      combine 4 5 -- 18 for 01

      -- to get 13 17
      combine 5 10
      combine 6 11
      combine 7 12
      combine 9 13
      combine 5 13
      combine 10 14
      combine 11 15
      combine 12 16
      combine 13 17 -- 27 for 10

      combine 5 27 -- 28 for 10

      -- to get 6 18
      combine 3 12
      combine 1 24
      combine 0 25
      combine 5 26
      combine 6 28 -- 33 for 10

      -- to get 7 33
      combine 1 29
      combine 0 30
      combine 5 31
      combine 6 32
      combine 7 33 -- 38 for 10

      combine 10 38 -- 39 for 10

      combine 8 18 -- 40 for 01

      combine 10 37
      combine 11 39 -- 42 for 10

      combine 4 35
      combine 10 36
      combine 11 41
      combine 12 42 -- 46 for 10

      return ()
    lifting = execState combiner (fromLGraph big5I big5)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    --easyLiftedGraphReport lifting
    --putChar '\n'
    print $ Spiral.fromHub intGraphI ig [Zero,One] [40,46]
    putChar '\n'
    --mapM_ (\c -> putStrLn (prettyCanWithArcs c) >> putChar '\n') cans
    --mapM_ (putStrLn . prettyCandidate) cans
    --putChar '\n'
    print pairs

gameAlloc3 :: IO ()
gameAlloc3 = let
    combiner = do
      combine 2 3
      combine 0 1 -- 5 and 1 univ
      -- end of forced
      combine 0 3
      combine 2 6
      combine 1 7
      combine 4 8
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
      -- at 0001 creating 001 universal
      combine 0 1 -- 16
      -- at 1110 creating 110 universal
      combine 14 15 -- 17

      -- wrap up 001 creating 01 universal
      combine 2 3 -- 18
      combine 8 9 -- 19
      combine 16 18 -- 20 the 01 universal 0-refl and 1-seen by 19
      -- can I replace 16 with 0?

      -- wrap up 110 creating 10 universal
      combine 12 13 -- 21
      combine 6 7 -- 22
      combine 17 21 -- 23 the 10 universal 1-refl and 0-seen by 22
      -- can I replace 17 with 15?

      -- wrap up 01
      combine 4 5 -- 24
      combine 10 11 -- 25
      combine 22 24 -- 26
      combine 19 25 -- 27
      combine 20 26 -- 28
      combine 23 27 -- 29

      combine 28 29

    {-
    -- alternative ending:
      -- wrap up 001 creating 01 universal
      combine 2 3 -- 18
      combine 8 9 -- 19
      combine 0 18 -- 20 the 01 universal 0-refl and 1-seen by 19
      -- can I replace 16 with 0?

      -- wrap up 110 creating 10 universal
      combine 12 13 -- 21
      combine 6 7 -- 22
      combine 17 21 -- 23 the 10 universal 1-refl and 0-seen by 22
      -- can I replace 17 with 15?

      -- wrap up 01
      combine 4 5 -- 24
      combine 10 11 -- 25
      combine 22 24 -- 26
      combine 19 25 -- 27
      combine 0 26 -- 28
      combine 23 27 -- 29


      combine 0 16 -- 30
      combine 20 30 -- 31
      combine 28 31 -- 32
      combine 29 32
    -}

      return ()
    lifting = execState combiner (fromLGraph dbgI (dbg 4))
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    print pairs


gameStudyBiggest :: IO ()
gameStudyBiggest = let
    combiner = do
      combine 0 3 -- 4
      combine 0 2 -- 5
      combine 1 2 -- 6
      combine 1 3 -- 7
      combine 2 4 -- 8
      combine 2 3 -- 9

      combine 1 3 -- 10
      combine 0 1 -- 11
      combine 2 4 -- 12
      combine 1 5 -- 13

      combine 2 3 -- 14
      combine 2 11 -- 15
      combine 1 14 -- 16
      combine 3 15 -- 17

      combine 4 16
      combine 5 17
      combine 6 18
      combine 4 19
      combine 12 20
      combine 5 22

      return ()
    lifting = execState combiner (fromLGraph biggestI biggest)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    easyLiftedGraphRelReport lifting [Zero,One]
    putChar '\n'
    print pairs
    --easyPathReport intGraphI ig
    --mapM_ (putStrLn . prettyCandidate) cans


gameStudy :: IO ()
gameStudy = let
    combiner = do

      return ()
    lifting = execState combiner (fromLGraph biggestI biggest)
    ig = graph lifting
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)
    pairs = map extractPair cans
  in do
    putStrLn $ unlines $ prettyLiftedGraph lifting
    putChar '\n'
    --easyLiftedGraphRelReport lifting [Zero,One]
    --putChar '\n'
    easyLiftedGraphRelReport lifting [One,One,Zero,Zero,One,Zero]
    putChar '\n'
    --easyLiftedGraphReport lifting
    --putChar '\n'
    --easyWordReport 15 intGraphI ig
    --putChar '\n'
    --print $ Spiral.fromHub intGraphI ig [Zero,One] [2,0]
    --putChar '\n'
    print $ Spiral.fromHub intGraphI ig [One,One,One,Zero,One,Zero] [0,2,1,3,2,1]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [One,One,One,Zero,One,Zero] [3,3,3,0,2,1]
    putChar '\n'
    print $ Spiral.fromHub intGraphI ig [One,One,One,Zero,One,Zero] [3,3,3,3,2,1]
    putChar '\n'
    print pairs
    --easyPathReport intGraphI ig
    --mapM_ (putStrLn . prettyCandidate) cans

