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
  describe "1-predecessors of 0 are above 1" $ do
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
  describe "words are reasonable" $ do
    it "001 has [001]" $
      turningWord (p0 . p0 $ one) `shouldBe` [Zero,Zero,One]
    let node = p0 . p1 . p1 . p0 . p0 $ one
    it "0110 at right positions" $
      turningWord node `shouldBe` [Zero,One,One,Zero]
    it "parent of 0110 has [011]" $
      (turningWord . parent $ node) `shouldBe` [Zero,One,One]
    it "0-pred of 0110 is above 001" $
      (turningWord . parent . p0 $ node) `shouldBe` [Zero,Zero,One]
  describe "110-loop" $ do
    it "110 is at 01101" $
      turningWord (p1 . p1 $ zero) `shouldBe` [One,One,Zero]
    it "101 is at 01101" $
      turningWord (p1 . p0 . p1 . p1 $ zero) `shouldBe` [One,Zero,One]
    it "101 <1 110" $
      p1 (p1 . p0 . p1 . p1 $ zero) `shouldBe` (p1 . p1 $ zero)
  describe "the difficult case for ascent trees" $ do
    let node = p0 . p1 . p0 . p0 . p1 . p0 . p1 . p0 . p0 $ one
    it "01001 is at 1001010010" $
      turningWord node `shouldBe` [Zero,One,Zero,Zero,One]
    it "its parent has word 010" $
      turningWord (parent node) `shouldBe` [Zero,One,Zero]
    it "its 0-predecessor has word 00100101" $
      turningWord (p0 node) `shouldBe` [Zero,Zero,One,Zero,Zero,One,Zero,One]
  describe "checking addresses" $ do
    it "of 01" $
      address (p0 $ one) `shouldBe` [One,Zero]
    it "of 1001" $
      address (p1 . p0 . p0 . p1 . p1 $ zero) `shouldBe` [Zero,One,One,Zero,Zero,One]
  describe "lookup at addresses" $ do
    it "011 is 110" $
      lookupAddress [Zero,One,One] `shouldBe` (p1 . p1 $ zero)
    it "0101 is 10" $
      lookupAddress [Zero,One,Zero,One] `shouldBe` (p1 zero)
  describe "lookupAdress . address = id " $ do
    it "on epsilon" $
      (lookupAddress . address $ epsilon) `shouldBe` epsilon
    it "on one" $
      (lookupAddress . address $ one) `shouldBe` one
    it "0101 is 10" $
      (lookupAddress . address $ (p1 . p1 . p0 . p0 $ one)) `shouldBe` (p1 . p1 . p0 . p0 $ one)
  describe "address . lookupAddress = shortes address" $ do
    it "on []" $
      (address . lookupAddress $ [])`shouldBe` []
    it "on [One,One,Zero,Zero,Zero,One]" $
      (address . lookupAddress $ [One,One,Zero,Zero,Zero,One]) `shouldBe` [One,Zero,Zero,Zero,One]


