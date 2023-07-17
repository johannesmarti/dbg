module ConciseGraphSpec (
   spec
) where

import Test.Hspec

import Graphs.ConciseGraph
import Examples.Patterns

spec :: Spec
spec = do
  describe "checking conversion between old and new codes" $ do
    describe "toCode . fromCode = id" $ do
      it "on 4928301 of size 4" $
        toCode 4 (fromCode 4 4928301) `shouldBe` 4928301
      it "on 29384 of size 4" $
        toCode 3 (fromCode 3 29384) `shouldBe` 29384
    describe "fromCode . toCode = id" $ do
      it "on cayleySchreck" $
        fromCode cayleySchreckSize (toCode cayleySchreckSize cayleySchreck) `shouldBe` cayleySchreck
      it "on slowFourConcise" $
        fromCode slowFourConciseSize (toCode slowFourConciseSize slowFourConcise) `shouldBe` slowFourConcise
  describe "size estimations" $ do
    it "size of cayleySchreck" $
      estimateSize (toCode cayleySchreckSize cayleySchreck) `shouldBe` cayleySchreckSize
    it "size of big5" $
      estimateSize (toCode 5 big5) `shouldBe` 5
