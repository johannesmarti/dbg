module Test.CaleyGraphSpec (
   spec
) where

import Test.Hspec

import CaleyGraph
import ConciseGraph
import CommonLGraphTypes
import LabeledGraph
import Patterns

caleyGraphOfConcise :: Size -> ConciseGraph -> CaleyGraph
caleyGraphOfConcise size = (caleyGraphOfLBitGraph size) . (toLBitGraph size)

spec :: Spec
spec = do
  describe "caleyCondition" $ do
    it "caleySchreck is not good" $
      (caleyGraphOfConcise caleySchreckSize caleySchreck) `shouldSatisfy` (not . (isReallyGood caleySchreckSize))
    it "4003476 of size 4 is good" $
      (caleyGraphOfConcise 4 4003476) `shouldSatisfy` (isReallyGood 4)
