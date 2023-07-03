module Main (
  main
) where

import Patterns
import Plan
import LiftedGraph
import Label

force3dPlan :: Plan Char
force3dPlan =
  insert [Zero] (spoke 'a' []) $
  insert [One] (spoke 'b' []) $
  insert [Zero,One] (spoke 'a' [('b', 1)]) $
  insert [One,Zero] (spoke 'c' []) $
  insert [Zero,One,One] (spoke 'b' []) $
  insert [Zero,One,One,Zero] (spoke 'c' []) $
  insert [Zero,One,One,Zero,One] (spoke 'a' []) $
  insert [One,Zero,Zero] (spoke 'a' []) $
  insert [One,Zero,Zero,One] (spoke 'b' []) $
  insert [One,Zero,Zero,One,Zero] (spoke 'c' []) $ empty

main :: IO ()
main = do
  let (lg,dsl) = executePlan force3dI force3d force3dPlan
  print dsl
  putStrLn $ unlines $ prettyLiftedGraph lg
