module PlanSpec (
   spec
) where

import Test.Hspec

import Plan
import qualified Data.Map.Strict as Map
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

spec :: Spec
spec = do
  describe "nothing to do here" $ do
    return ()
