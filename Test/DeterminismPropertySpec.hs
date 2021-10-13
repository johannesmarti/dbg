module Test.DeterminismPropertySpec (
   spec
) where

import Data.Maybe
import Data.Set
import Test.Hspec

import DeBruijn
import Graph
import Patterns
import MapGraph
import DeterminismProperty


spec :: Spec
spec = do
  describe "partiton is working" $ do
    let pDis = discrete (fromList [1..6])
    it "discrete is not trivial" $
      pDis `shouldSatisfy` (not . isTrivial)
    let p1 = identify (identify pDis 1 2) 2 3
    it "identifications are transitive" $
      representative p1 1 `shouldBe` representative p1 3
    it "non-identify shold be distinct" $
      (representative p1 2 == representative p1 4) `shouldBe` False
    let p2 = identify (identify p1 2 4) 5 6
    it "classes are the same" $
      eqClass p2 1 `shouldBe` eqClass p2 4
    it "classes should stay distict" $
      (eqClass p2 2 == eqClass p2 5) `shouldBe` False
    it "with two classes we are not trivial" $
      p2 `shouldSatisfy` (not . isTrivial)
    let pt = (identify p2 3 5)
    it "trivial after identifying everything" $
      pt `shouldSatisfy` isTrivial
  describe "has Determinism Property" $ do
    it "hamburger" $
      hasDeterminismProperty mapGraphI hamburger (domain mapGraphI hamburger)
        `shouldSatisfy` isJust
  describe "does not have Determinism Property" $ do
    it "dbg 3" $
      hasDeterminismProperty dbgI (dbg 3) (domain dbgI (dbg 3))
        `shouldSatisfy` isNothing
    it "force3d" $
      hasDeterminismProperty mapGraphI force3d (domain mapGraphI force3d)
        `shouldSatisfy` isNothing
