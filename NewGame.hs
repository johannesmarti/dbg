module NewGame (
  game,
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

