module PlanSpec (
   spec
) where

import Test.Hspec

import Plan
import qualified Data.Map.Strict as Map
--import Control.Monad.State.Lazy
import qualified Data.Vector as V
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
  describe "testing wrapSpiral" $ do
    it "wrap up a simple spiral" $
      undefined `shouldBe` undefined -- wrap up a simple spiral
