module Main (
  main
) where

import ExecutePlan
import LiftedGraph
import Data.Label
import Examples.Patterns
import Examples.Plans

main :: IO ()
main = do
  --let (lg,dsl) = executePlan alloc3I alloc3 alloc3Plan
  let (lg,dsl) = executePlan force3dI force3d force3dPlan
  print dsl
  putStrLn $ unlines $ prettyLiftedGraph lg
