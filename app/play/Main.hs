module Main (
  main
) where

import Plan
import LiftedGraph
import Label
import Examples.Patterns
import Examples.Plans

main :: IO ()
main = do
  --let (lg,dsl) = executePlan alloc3I alloc3 alloc3Plan
  let (lg,dsl) = executePlan force3dI force3d force3dPlan
  print dsl
  putStrLn $ unlines $ prettyLiftedGraph lg
