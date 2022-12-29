module GatheringSpec (
   spec
) where

import Test.Hspec

import Descent
import Gathering
import Path
import Label


spec :: Spec
spec = do
  describe "working with an ascent path" $ do
    let dt = descentTreeForBound 64
    let path = generateAscentPath dt [Zero,Zero,One] [Zero,Zero,One,One]
    let (Step first l1 (Step second l2 (Step third l3 _))) = path
    it "first element is the one we generated from" $ do
      first `shouldBe` [Zero,Zero,One,One]
    it "second is [One,Zero,Zero,One]" $ do
      second `shouldBe` [One,Zero,Zero,One]
    it "third element is [Zero,One,Zero,Zero,One,One]" $ do
      third `shouldBe` [Zero,One,Zero,Zero,One,One]
    it "first label is One" $ do
      l1 `shouldBe` One
    it "second label is Zero" $ do
      l2 `shouldBe` Zero
    it "third label is Zero" $ do
      l3 `shouldBe` Zero
    let shorterPath = generateAscentPath dt [Zero,Zero,One] third
    it "path is longer than shorter path" $ do
      (path `longer` shorterPath) `shouldBe` True
    it "shorter path is not longer than path" $ do
      (shorterPath `longer` path) `shouldBe` False
