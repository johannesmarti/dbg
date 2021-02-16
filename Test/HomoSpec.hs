module Test.HomoSpec (
   spec
) where

import Data.Set as Set
import Test.Hspec

import DeBruijn
import Graph
import Homo
import Patterns

db2 = deBruijnGraph 2
db3 = deBruijnGraph 3

spec :: Spec
spec = do
  describe "no homo" $ do
    it "from db2 to db3" $
      searchHomos db2 db3 `shouldBe` []
    it "from db2 to triple" $
      searchHomos db2 triple `shouldBe` []
  describe "unique homo" $ do
    it "from db2 to db2" $
      length (searchHomos db2 db2) `shouldBe` 1
    it "from db3 to db2" $
      length (searchHomos db3 db2) `shouldBe` 1
