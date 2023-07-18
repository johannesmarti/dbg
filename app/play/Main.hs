module Main (
  main
) where

import Plans.Execute
import Lifting.CombinationGraph
import Examples.Patterns
import Examples.Plans

main :: IO ()
main = do
  --let (lg,dsl) = executePlan alloc3Interface alloc3 alloc3Plan
  let (lg,dsl) = executePlan force3dInterface force3d force3dPlan
  print dsl
  putStrLn $ unlines $ prettyCombinationGraph lg
