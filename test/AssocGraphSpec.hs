module AssocGraphSpec (
   spec
) where

import Data.Set as Set
import Test.Hspec

import CommonLGraphTypes
import LabeledGraph

f1 Zero = [(1,2),(1,1),(2,5)]
f1 One = [(5,5)]

g :: LAssocGraph Int
g = assocFromFunction f1

spec :: Spec
spec = do
  describe "simple graph g" $ do
    it "0-sucss of 1 match [1,2]" $
      successors lAssocGraphI g Zero 1 `shouldBe` Set.fromList [1,2]
    it "0-pred of 5 match [2]" $
      predecessors lAssocGraphI g Zero 5 `shouldBe` Set.fromList [2]
