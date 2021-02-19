module Test.ArcConsSpec (
   spec
) where

import Data.Set as Set
import Test.Hspec

import ArcCons
import DeBruijn
import Graph
import Patterns

db1 = deBruijnGraph 1
db2 = deBruijnGraph 2
db3 = deBruijnGraph 3

spec :: Spec
spec = do
  describe "no homo" $ do
    it "from db2 to db3" $
      arcConsHomos db2 db3 `shouldBe` []
    it "from db2 to triple" $
      arcConsHomos db2 triple `shouldBe` []
    it "from db1 to force2" $
      arcConsHomos db1 force2 `shouldBe` []
  describe "unique homo" $ do
    it "from db2 to db2" $
      length (arcConsHomos db2 db2) `shouldBe` 1
    it "from db3 to db2" $
      length (arcConsHomos db3 db2) `shouldBe` 1
    it "from db2 to force2" $
      length (arcConsHomos db2 force2) `shouldBe` 1
    it "from db3 to force2" $
      length (arcConsHomos db3 force2) `shouldBe` 1
