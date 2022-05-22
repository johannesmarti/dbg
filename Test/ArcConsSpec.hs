module Test.ArcConsSpec (
   spec
) where

import Data.Map as Map
import Data.Set as Set
import Test.Hspec

import ArcCons
import DeBruijnGraph
import LabeledGraph
import CommonLGraphTypes
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

interface :: Spec
interface = do
  describe "no homo" $ do
    it "from db2 to db3" $
      arcConsHomomorphisms dbgI dbgI (dbg 2) (dbg 3) `shouldBe` []
    it "from db2 to triple" $
      arcConsHomomorphisms dbgI lMapGraphI (dbg 2) triple `shouldBe` []
    it "from db1 to force2d" $
      arcConsHomomorphisms dbgI lMapGraphI (dbg 1) force2d `shouldBe` []
  describe "unique homo" $ do
    it "from db2 to db2" $
      length (arcConsHomomorphisms dbgI dbgI (dbg 2) (dbg 2)) `shouldBe` 1
    it "from db3 to db2" $
      length (arcConsHomomorphisms dbgI dbgI (dbg 3) (dbg 2)) `shouldBe` 1
    it "from db2 to force2d" $
      length (arcConsHomomorphisms dbgI lMapGraphI (dbg 2) force2d) `shouldBe` 1
    it "from db3 to force2d" $
      length (arcConsHomomorphisms dbgI lMapGraphI (dbg 3) force2d) `shouldBe` 1
    it "from db3 to force3d" $
      length (arcConsHomomorphisms dbgI lMapGraphI (dbg 3) force2d) `shouldBe` 1
