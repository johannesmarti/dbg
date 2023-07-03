module TurningVectorSpec (
   spec
) where

import Test.Hspec

import qualified TurningVector as TV


spec :: Spec
spec = do
  let v1 = TV.fromList [1,2,3,4,5,6,7]
  describe "checks on v1 = [1,2,3,4,5,6,7]" $ do
    it "element at 0 is 1" $
      v1 `TV.at` 0 `shouldBe` 1
    it "element at 4 is 5" $
      v1 `TV.at` 4 `shouldBe` 5
    it "element at 8 is 2" $
      v1 `TV.at` 8 `shouldBe` 2
  let v2 = TV.turnBackward . TV.turnBackward $ v1
  describe "checks on turnBackward . turnBackard $ v1 = [6,7,1,2,3,4,5]" $ do
    it "element at 0 is 6" $
      v2 `TV.at` 0 `shouldBe` 6
    it "element at 4 is 3" $
      v2 `TV.at` 4 `shouldBe` 3
    it "toList v2 = [6,7,1,2,3,4,5]" $
      TV.toList v2 `shouldBe` [6,7,1,2,3,4,5]
    it "turnBackward (turnForward v2) = [6,7,1,2,3,4,5]" $
      (TV.toList . TV.turnBackward . TV.turnForward $ v2) `shouldBe` [6,7,1,2,3,4,5]

