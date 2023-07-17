module BitGraphSpec (
   spec
) where

import Test.Hspec

import Graphs.BitGraph

spec :: Spec
spec = do
  describe "checking hasNoRefl on examples" $ do
    it "nullWord does not have refl" $
      hasNoRefl 3 nullWord `shouldBe` True
    it "totalGraph has refl" $
      hasNoRefl 3 (totalGraph 3) `shouldBe` False
    it "1010 has refl 2" $
      hasNoRefl 2 10  `shouldBe` False
    it "0110 has no refl 2" $
      hasNoRefl 2 6 `shouldBe` True
  describe "checking hasUniv on examples" $ do
    it "nullWord does not have Univ" $
      hasUniv 3 nullWord `shouldBe` False
    it "totalGraph has univ" $
      hasUniv 3 (totalGraph 3) `shouldBe` True
    it "0011 has univ in size 2" $
      hasUniv 2 3  `shouldBe` True
    it "1100 has univ in size 2" $
      hasUniv 2 12  `shouldBe` True
    it "1011 has univ in size 2" $
      hasUniv 2 11  `shouldBe` True
    it "1010 has no univ in size 2" $
      hasUniv 2 10  `shouldBe` False
    it "000000111 has univ in size 3" $
      hasUniv 3 7  `shouldBe` True
    it "000001000 has no univ in size 3" $
      hasUniv 3 8  `shouldBe` False
  describe "checking hasReflAndUnivInMultiple on examples" $ do
    it "000000111 has refl and univ in size 3" $
      hasReflAndUnivInMultiple 3 7  `shouldBe` True
    it "000001000 has no refl and univ in size 3" $
      hasReflAndUnivInMultiple 3 8  `shouldBe` False
    it "000100011 has refl and univ in size 3" $
      hasReflAndUnivInMultiple 3 35  `shouldBe` True
  describe "checking hasReflAndUnivInMultipleDom on examples" $ do
    it "000000111 has refl and univ in size 3 and dom [0,1]" $
      hasReflAndUnivInMultipleDom 3 [0,1] 7  `shouldBe` True
    it "000001000 has no refl and univ in size 3 and dom [0,2]" $
      hasReflAndUnivInMultipleDom 3 [0,2] 8  `shouldBe` False
    it "000010000 has refl and univ in size 3 and dom [1]" $
      hasReflAndUnivInMultipleDom 3 [1] 16  `shouldBe` True
    it "000000011 has refl and univ in size 3 and dom [0,1]" $
      hasReflAndUnivInMultipleDom 3 [0,1] 3  `shouldBe` True
    it "000000011 has no refl and univ in size 3 and dom [0,2]" $
      hasReflAndUnivInMultipleDom 3 [0,2] 3  `shouldBe` False
  describe "checking some compositons" $ do
    it "5 . top = 7 (in 3)" $
      compose 3 5 (totalGraph 3) `shouldBe` 7
    it "8 . bot = bot" $
      compose 3 8 nullWord `shouldBe` nullWord
    it "1110 . 1110 = top" $
      compose 2 14 14 `shouldBe` (totalGraph 2)
    it "0110 . 0011 = 1100" $
      compose 2 6 3 `shouldBe` 12
