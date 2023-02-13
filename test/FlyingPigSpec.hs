module FlyingPigSpec (
   spec
) where

import Test.Hspec

import FlyingPig
import Label


spec :: Spec
spec = do
  let p0 = predecessor Zero
  let p1 = predecessor One
  describe "checking predecessors of simple objects" $ do
    it "0-predecessor of epsilon is zero" $
      p0 epsilon `shouldBe` zero
    it "1-predecessor of epsilon is one" $
      p1 epsilon `shouldBe` one
    it "0-predecessor of zero is zero" $
      p0 zero `shouldBe` zero
    it "1-predecessor of one is one" $
      p1 one `shouldBe` one
  describe "predecessors of 0 are above 1" $ do
    it "for 10" $
      parent (p1 zero) `shouldBe` one
    it "for 110" $
      parent (p1 . p1 $ zero) `shouldBe` one
    it "for 11110" $
      parent (p1 . p1 . p1 . p1 $ zero) `shouldBe` one
  describe "01-loop" $ do
    it "10 <0 01" $
      p0 (p1 zero) `shouldBe` (p0 one)
    it "01 <1 10" $
      p1 (p0 one) `shouldBe` (p1 zero)
{-
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
-}
