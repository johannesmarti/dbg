module HomomorphismSpec (
   spec
) where

import Data.Set as Set
import Test.Hspec

import DeBruijnGraph
import LabeledGraph
import CommonLGraphTypes
import Homomorphism
import Examples.Patterns

spec :: Spec
spec = do
  describe "no homo" $ do
    it "from db2 to db3" $
      searchHomomorphisms dbgI dbgI (dbg 2) (dbg 3) `shouldBe` []
    it "from db2 to triple" $
      searchHomomorphisms dbgI lMapGraphI (dbg 2) triple `shouldBe` []
  describe "unique homo" $ do
    it "from db2 to db2" $
      length (searchHomomorphisms dbgI dbgI (dbg 2) (dbg 2)) `shouldBe` 1
    it "from db3 to db2" $
      length (searchHomomorphisms dbgI dbgI (dbg 3) (dbg 2)) `shouldBe` 1
