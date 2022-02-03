module Test.CaleyGraphSpec (
   spec
) where

import Test.Hspec

import Bitify
import CaleyGraph
import ConciseGraph
import CommonLGraphTypes
import LabeledGraph
import Patterns

spec :: Spec
spec = do
  describe "caleyCondition" $ do
    it "caleySchreck is does not have path condition" $
      (caleyGraphOfConcise caleySchreckSize caleySchreck) `shouldSatisfy` (not . (pathCondition caleySchreckSize))
    it "4003476 of size 4 has path condition" $
      (caleyGraphOfConcise 4 4003476) `shouldSatisfy` (pathCondition 4)
    it "noPath of size 4 does not have path condition" $
      noPath `shouldSatisfy` (not . (hasPathCondition noPathI))
    it "notQuitePath of size 4 does not have path condition" $
      notQuitePath `shouldSatisfy` (not . (hasPathCondition notQuitePathI))
