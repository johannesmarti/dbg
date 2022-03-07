module NewGame (
  game,
) where

import Control.Monad.State.Lazy

import LiftedGraph
import Patterns

actualGame :: State (LiftedGraph x) [LiftingCandidate Int]
actualGame = do
  combine 4 7
  combine 5 9
  lg <- get
  return (liftableCandidates intGraphI (graph lg))

game :: IO ()
game = do
  let (cans,lifting) = runState actualGame (fromLGraph ex5I ex5)
  putStrLn $ unlines $ prettyLiftedGraph lifting
  --mapM_ (putStrLn . show) cans

