module Test.ArcConsSpec (
   spec
) where

import Data.Map as Map
import Data.Set as Set
import Test.Hspec

import ArcCons
import DeBruijn
import Graph
import Patterns

spec :: Spec
spec = do
  describe "checking the internals" details
  describe "now checking the interface" interface

easyMap = Map.map Set.fromList . Map.fromList
ap1 = easyMap [(3,[6]),(2,[7])]
ap2 = easyMap [(1,[2]),(3,[1,2,3]),(12,[99])]

details :: Spec
details =
  describe "hasSplit" $ do
    it "[] has no split" $
      hasSplit (easyMap []) `shouldBe` Nothing
    it "[(3,[6]),(2,[7])] has no split" $
      hasSplit ap1 `shouldBe` Nothing
    it "[(1,[2]),(3,[1,2,3]),(12,[99])] has split at 3" $
      hasSplit ap2 `shouldBe` Just 3

db1 = deBruijnGraph 1
db2 = deBruijnGraph 2
db3 = deBruijnGraph 3

interface :: Spec
interface = do
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
