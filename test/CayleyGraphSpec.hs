module CayleyGraphSpec (
   spec
) where

import Test.Hspec

import Bitify
import CayleyGraph
import ConciseGraph
import CommonLGraphTypes
import LabeledGraph
import Patterns

spec :: Spec
spec = do
  describe "cayleyCondition" $ do
    it "cayleySchreck is does not have path condition" $
      (cayleyGraphOfConcise cayleySchreckSize cayleySchreck) `shouldSatisfy` (not . (pathCondition cayleySchreckSize))
    it "4003476 of size 4 has path condition" $
      (cayleyGraphOfConcise 4 4003476) `shouldSatisfy` (pathCondition 4)
    it "noPath of size 4 does not have path condition" $
      noPath `shouldSatisfy` (not . (hasPathCondition noPathI))
    it "notQuitePath of size 4 does not have path condition" $
      notQuitePath `shouldSatisfy` (not . (hasPathCondition notQuitePathI))
