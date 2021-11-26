module Test.HomoSpec (
   spec
) where

import Data.Set as Set
import Test.Hspec

import DeBruijnGraph
import LabeledGraph
import CommonLGraphTypes
import Homo
import Patterns

spec :: Spec
spec = do
  describe "no homo" $ do
    it "from db2 to db3" $
      searchHomos dbgI dbgI (dbg 2) (dbg 3) `shouldBe` []
    it "from db2 to triple" $
      searchHomos dbgI lMapGraphI (dbg 2) triple `shouldBe` []
  describe "unique homo" $ do
    it "from db2 to db2" $
      length (searchHomos dbgI dbgI (dbg 2) (dbg 2)) `shouldBe` 1
    it "from db3 to db2" $
      length (searchHomos dbgI dbgI (dbg 3) (dbg 2)) `shouldBe` 1
