module LiftedSpec (
   spec
) where

import Control.Exception (evaluate)
import Test.Hspec

import Lifted

rel :: Char -> Char -> Bool
rel 'a' 'a' = False
rel 'a' 'b' = False
rel 'a' 'c' = False
rel 'b' 'a' = True
rel 'b' 'b' = False
rel 'b' 'c' = False
rel 'c' 'a' = True
rel 'c' 'b' = True
rel 'c' 'c' = True

spec :: Spec
spec = do
  let (a,b,c) = (bn 'a', bn 'b', bn 'c')
  let (sa,sb,sc) = (si a, si b, si c)
  let dab = du a b
  let dac = du a c
  let dsadbc = du sa (du b c)
  describe "construction of basic lifted elements" $ do
    it "order in doubletons does not matter" $
       (du a b) `shouldBe` (du b a)
    it "[a b] is not equal to [a c]" $
       (du a b) `shouldNotBe` (du a c)
    it "deepening deepens" $
       (deepen (du a b)) `shouldBe` (du sb sa)
    it "depth of double deepening is 3" $
       (depth (deepen (deepen (du a b)))) `shouldBe` 3
    it "should not be able to construct an unbalanced element" $
      (evaluate (du a sb)) `shouldThrow` anyException
    it "du' throws exception if elements are not in order" $
      (evaluate (du (du' a b) (du' b a))) `shouldThrow` anyException
  describe "lifted relation works" $ do
    it "some basic lifts of a relation are as expected" $ do
       ((du a b),(si a)) `shouldSatisfy` (\(x,y) -> liftedRelation rel x y)
       ((du a b),(si c)) `shouldNotSatisfy` (\(x,y) -> liftedRelation rel x y)
       ((si b),(si a)) `shouldSatisfy` (\(x,y) -> liftedRelation rel x y)
       ((si b),(du a b)) `shouldNotSatisfy` (\(x,y) -> liftedRelation rel x y)
       ((si c),(si a)) `shouldSatisfy` (\(x,y) -> liftedRelation rel x y)
       ((si c),(du a b)) `shouldSatisfy` (\(x,y) -> liftedRelation rel x y)
       ((du a b),(du a b)) `shouldNotSatisfy` (\(x,y) -> liftedRelation rel x y)
       ((du a c),(du a b)) `shouldSatisfy` (\(x,y) -> liftedRelation rel x y)
       ((du a c),(du c b)) `shouldSatisfy` (\(x,y) -> liftedRelation rel x y)
    it "some higher lifts of a relation are as expected" $ do
       (dsadbc,dsadbc) `shouldSatisfy` (\(x,y) -> liftedRelation rel x y)
       ((du sa dab),dsadbc) `shouldNotSatisfy` (\(x,y) -> liftedRelation rel x y)
    it "throws exception for unbalanced comparisons" $ do
      (evaluate (liftedRelation rel dsadbc dab)) `shouldThrow` anyException
      (evaluate (liftedRelation rel dac dsadbc)) `shouldThrow` anyException
