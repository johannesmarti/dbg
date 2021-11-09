module Test.CaleyGraphSpec (
   spec
) where

import Test.Hspec

import CaleyGraph
import ConciseGraph
import Graph
import Patterns

spec :: Spec
spec = do
  describe "caleyCondition" $ do
    it "caleySchreck is not good" $
      (caleyGraph caleySchreckSize caleySchreck) `shouldSatisfy` (not . (isReallyGood caleySchreckSize))
    it "4003476 of size 4 is good" $
      (caleyGraph 4 4003476) `shouldSatisfy` (isReallyGood 4)