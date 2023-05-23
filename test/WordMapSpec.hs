module WordMapSpec (
   spec
) where

import Test.Hspec

import Label
import qualified WordMap.Algebraic as Algebraic
import qualified WordMap.Coalgebraic as Coalgebraic
import qualified WordMap.MapWrapper as MapWrapper

data WordMapI m x = WordMapI {
  empty   :: m,
  combine :: x -> m -> m -> m,
  lookup  :: [Label] -> m -> Maybe x,
  insert  :: [Label] -> x -> m -> m,
  delete  :: [Label] -> m -> m
}

algebraicI :: WordMapI (Algebraic.WordMap x) x
algebraicI = WordMapI
  Algebraic.empty
  Algebraic.combine
  Algebraic.lookup
  Algebraic.insert
  Algebraic.delete

coalgebraicI :: WordMapI (Coalgebraic.WordMap x) x
coalgebraicI = WordMapI
  Coalgebraic.empty
  Coalgebraic.combine
  Coalgebraic.lookup
  Coalgebraic.insert
  Coalgebraic.delete

mapWrapperI :: WordMapI (MapWrapper.WordMap x) x
mapWrapperI = WordMapI
  MapWrapper.empty
  MapWrapper.combine
  MapWrapper.lookup
  MapWrapper.insert
  MapWrapper.delete

spec :: Spec
spec = do
  wordMapSpecOn "WordMap.Algebraic" algebraicI
  wordMapSpecOn "WordMap.Coalgebraic" coalgebraicI
  wordMapSpecOn "WordMap.MapWrapper" mapWrapperI

wordMapSpecOn :: String -> WordMapI m Int -> Spec
wordMapSpecOn name wmi =
  describe ("checking implementation of WordMap by " ++ name) $ do
    let em = empty wmi
    let comb = WordMapSpec.combine wmi
    let look = WordMapSpec.lookup wmi
    let ins  = WordMapSpec.insert wmi
    let del  = WordMapSpec.delete wmi
    describe "tests on empty map" $ do
      it "no value at []" $
        look [] em `shouldBe` Nothing
      it "no value at [Zero]" $
        look [Zero] em `shouldBe` Nothing
      it "no value at [One,Zero,One,Zero,Zero]" $
        look [One,Zero,One,Zero,Zero] em `shouldBe` Nothing
    let something = ins [One,Zero,One] 5 em
    describe "insert 5 at [One,Zero,One] of empty" $ do
      it "value 5 at [One,Zero,One]" $
        look [One,Zero,One] something `shouldBe` Just 5
      it "no value at [One,Zero,One,Zero,Zero]" $
        look [One,Zero,One,Zero,Zero] something `shouldBe` Nothing
    let stillSomething = del [One,Zero] something
    describe "delete value at [One,Zero]" $ do
      it "still value 5 at [One,Zero,One]" $
        look [One,Zero,One] stillSomething `shouldBe` Just 5
      it "no value at [One,Zero,One,Zero,Zero]" $
        look [One,Zero,One,Zero,Zero] stillSomething `shouldBe` Nothing
    let againNothing = del [One,Zero,One] stillSomething
    describe "delete value 5 at [One,Zero,One]" $ do
      it "still value 5 at [One,Zero,One]" $
        look [One,Zero,One] againNothing `shouldBe` Nothing
      it "no value at [Zero]" $
        look [Zero] againNothing `shouldBe` Nothing
      it "no value at [One,Zero,One,Zero,Zero]" $
        look [One,Zero,One,Zero,Zero] againNothing `shouldBe` Nothing
    let complex = comb 0
                    (comb 1
                       (comb 2 em em)
                       (comb 3 em (comb (-3) em em)))
                    (comb 4
                       (comb 5
                          (comb 6 em em)
                          (comb 7 (comb 1 em em) (comb 2 (comb (-2) em em) em)))
                       em)
    describe "checking relatively complex map [0 [1 [2 [] []] [3 [] [-3 [] []]]] [4 [5 [6 [] []] [7 [1 [] []] [2 [-2 [] []] []]]] []]]" $ do
      it "value 0 at []" $
        look [] complex `shouldBe` Just 0
      it "value -3 at [Zero,One,One]" $
        look [Zero,One,One] complex `shouldBe` Just (-3)
      it "no value at [One,Zero,One,Zero,Zero]" $
        look [One,Zero,One,Zero,Zero] againNothing `shouldBe` Nothing
      it "nothing at [One,One,Zero]" $
        look [One,One,Zero] complex `shouldBe` Nothing
    let smaller = del [One] complex
    describe "delete value at [One]" $ do
      it "value 0 at []" $
        look [] smaller `shouldBe` Just 0
      it "no value at [One]" $
        look [One] smaller `shouldBe` Nothing
      it "value 5 at [One,Zero]" $
        look [One,Zero] smaller `shouldBe` Just 5
      it "value -3 at [Zero,One,One]" $
        look [Zero,One,One] smaller `shouldBe` Just (-3)
      it "no value at [One,Zero,One,Zero,Zero]" $
        look [One,Zero,One,Zero,Zero] againNothing `shouldBe` Nothing
    let muchSmaller = del [Zero] $ del [Zero,Zero] $ del [Zero,One] smaller
    describe "delete [Zero], [Zero,Zero] and [Zero,One]" $ do
      it "value -3 at [Zero,One,One] is still there" $
        look [Zero,One,One] muchSmaller `shouldBe` Just (-3)
      it "value 2 at [Zero,Zero] is gone" $
        look [Zero,Zero] muchSmaller `shouldBe` Nothing
    let killLeft = del [Zero,One,One] muchSmaller
    describe "kill the whole left branch by deleting [Zero,One,One]" $ do
      it "value -3 at [Zero,One,One] is gone" $
        look [Zero,One,One] killLeft `shouldBe` Nothing
      it "value at [Zero,Zero] is still gone" $
        look [Zero,Zero] killLeft `shouldBe` Nothing
      it "value 2 at [One,Zero,One,One] is still there" $
        look [One,Zero,One,One] killLeft `shouldBe` Just 2
    let newStart = ins [One,Zero,One,One] 100 killLeft
    describe "insert 100 at [One,Zero,One,One]" $ do
      it "value -3 at [Zero,One,One] is gone" $
        look [Zero,One,One] newStart `shouldBe` Nothing
      it "value 2 at [One,Zero,One,One] is still there" $
        look [One,Zero,One,One] newStart `shouldBe` Just 100
      it "value 1 at [One,Zero,One,Zero] is still there" $
        look [One,Zero,One,Zero] newStart `shouldBe` Just 1
