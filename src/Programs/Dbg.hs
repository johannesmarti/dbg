module Programs.Dbg (
  dbg,
) where

import Lifting.CombinationGraph
import Plans.Execute
import Examples.Patterns
import Examples.Plans

import Programs.Play

dbg :: IO ()
dbg = bits

silly :: IO ()
silly = do
  --let (lg,dsl) = executePlan alloc3Interface alloc3 alloc3Plan
  let (lg,dsl) = executePlan force3dInterface force3d force3dPlan
  print dsl
  putStrLn $ unlines $ prettyCombinationGraph lg
