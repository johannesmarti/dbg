module AssocGraphSpec (
   spec
) where

import Data.Set as Set
import Test.Hspec

import CommonLabeledGraphTypes
import LabeledGraphInterface

f1 Zero = [(1,2),(1,1),(2,5)]
f1 One = [(5,5)]

g :: LabeledAssocGraph Int
g = assocFromFunction f1

spec :: Spec
spec = do
  describe "simple graph g" $ do
    it "0-sucss of 1 match [1,2]" $
      successors labeledAssocGraphInterface g Zero 1 `shouldBe` Set.fromList [1,2]
    it "0-pred of 5 match [2]" $
      predecessors labeledAssocGraphInterface g Zero 5 `shouldBe` Set.fromList [2]
