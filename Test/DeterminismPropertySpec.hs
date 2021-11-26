module Test.DeterminismPropertySpec (
   spec
) where

import Data.Maybe
import Data.Set
import Test.Hspec

import DeBruijnGraph
import LabeledGraph
import ConciseGraph
import Patterns
import CommonLGraphTypes
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
  describe "does have strong determinism property" $ do
    it "strongDet" $
      strongDet `shouldSatisfy` (isConstructionDeterministic lMapGraphI)
  describe "does not have strong determinism property" $ do
    it "dbg 3" $
      (dbg 3) `shouldSatisfy` (not . isConstructionDeterministic dbgI)
    it "force3d" $
      force3d `shouldSatisfy` (not . isConstructionDeterministic lMapGraphI)
    it "hamburger" $
      hamburger `shouldSatisfy` (not . isConstructionDeterministic lMapGraphI)
    it "strange3" $
      strange3 `shouldSatisfy` (not . isConstructionDeterministic lMapGraphI)
    it "4003476 of size 4" $
      4003476 `shouldSatisfy` (not . isConstructionDeterministic (conciseGraphI 4))
  describe "does have weak determinism property" $ do
    it "hamburger" $
      hamburger `shouldSatisfy` (isWeaklyConstructionDeterministic lMapGraphI)
    it "strange3" $
      strange3 `shouldSatisfy` (isWeaklyConstructionDeterministic lMapGraphI)
  describe "does not have weak determinism property" $ do
    it "dbg 1" $
      (dbg 1) `shouldSatisfy` (not . isWeaklyConstructionDeterministic dbgI)
    it "dbg 3" $
      (dbg 3) `shouldSatisfy` (not . isWeaklyConstructionDeterministic dbgI)
    it "force3d" $
      force3d `shouldSatisfy` (not . isWeaklyConstructionDeterministic lMapGraphI)
    it "4003476 of size 4" $
      4003476 `shouldSatisfy` (not . isWeaklyConstructionDeterministic (conciseGraphI 4))
