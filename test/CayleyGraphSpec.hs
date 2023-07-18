module CayleyGraphSpec (
   spec
) where

import Test.Hspec

import Bitify.Bitifier
import Conditions.CayleyGraph
import Examples.Patterns

spec :: Spec
spec = do
  describe "cayleyCondition" $ do
    let csb = conciseGraphBitifier cayleySchreckSize cayleySchreck
    it "cayleySchreck is does not have path condition" $
      (rightCayleyGraph csb) `shouldSatisfy` (not . (pathCondition (numBits csb)))

    it "4003476 of size 4 has path condition" $
      (rightCayleyGraph (conciseGraphBitifier 4 4003476)) `shouldSatisfy` (pathCondition 4)

    let noPathBitification = conciseGraphBitifier 4 noPath
    it "noPath of size 4 does not have path condition" $
      (rightCayleyGraph noPathBitification) `shouldSatisfy` (not . (pathCondition (numBits noPathBitification)))

    let notQuitePathBitification = conciseGraphBitifier 4 noPath
    it "notQuitePath of size 4 does not have path condition" $
      (rightCayleyGraph notQuitePathBitification) `shouldSatisfy` (not . (pathCondition (numBits notQuitePathBitification)))
