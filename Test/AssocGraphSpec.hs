module Test.AssocGraphSpec (
   spec
) where

import Data.Set as Set
import Test.Hspec

import AssocGraph
import Graph

f1 Zero = [(1,2),(1,1),(2,5)]
f1 One = [(5,5)]

g1 = toGraph f1

spec :: Spec
spec = do
  describe "simple graph g1" $ do
    it "0-sucss of 1 match [1,2]" $
      successors g1 Zero 1 `shouldBe` Set.fromList [1,2]
    it "0-pred of 5 match [2]" $
      predecessors g1 Zero 5 `shouldBe` Set.fromList [2]
