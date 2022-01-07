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
    it "non-identify should be distinct" $
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
  describe "is strictly construction deterministic" $ do
    it "strictDet" $
      strictDet `shouldSatisfy` (isStrictlyConstructionDeterministic lMapGraphI)
  describe "is not strictly construction deterministic" $ do
    it "dbg 3" $
      (dbg 3) `shouldSatisfy` (not . isStrictlyConstructionDeterministic dbgI)
    it "force3d" $
      force3d `shouldSatisfy` (not . isStrictlyConstructionDeterministic lMapGraphI)
    it "hamburger" $
      hamburger `shouldSatisfy` (not . isStrictlyConstructionDeterministic lMapGraphI)
    it "strange3" $
      strange3 `shouldSatisfy` (not . isStrictlyConstructionDeterministic lMapGraphI)
    it "4003476 of size 4" $
      4003476 `shouldSatisfy` (not . isStrictlyConstructionDeterministic (conciseGraphI 4))
  describe "is strongly construction determinism" $ do
    it "hamburger" $
      hamburger `shouldSatisfy` (isStronglyConstructionDeterministic lMapGraphI)
    it "strange3" $
      strange3 `shouldSatisfy` (isStronglyConstructionDeterministic lMapGraphI)
  describe "is not strongly construction deterministic" $ do
    it "dbg 1" $
      (dbg 1) `shouldSatisfy` (not . isStronglyConstructionDeterministic dbgI)
    it "dbg 3" $
      (dbg 3) `shouldSatisfy` (not . isStronglyConstructionDeterministic dbgI)
    it "force3d" $
      force3d `shouldSatisfy` (not . isStronglyConstructionDeterministic lMapGraphI)
    it "slowFourConcise" $
      slowFourConcise `shouldSatisfy` (not . isStronglyConstructionDeterministic (conciseGraphI 4))
    it "slowLifting" $
      slowLifting `shouldSatisfy` (not . isStronglyConstructionDeterministic slowLiftingI)
  describe "antichain is working" $ do
    let dom = fromList [1,2,3,4]
    let sA = singletonChain dom
    describe "singleton chain ..." $ do 
      it "... is not total" $
        sA `shouldSatisfy` (not . (isTotal dom))
      it "... covers [2]" $
        sA `shouldSatisfy` (`covers` (singleton 2))
      it "... does not cover [1,2]" $
        sA `shouldSatisfy` (\ac -> not (ac `covers` (fromList [1,2])))
    let bA = addProposition (fromList [1,2,3]) sA
    describe "singleton with added [1,2,3] ..." $ do 
      it "... is not total" $
        bA `shouldSatisfy` (not . (isTotal dom))
      it "... covers [1,2]" $
        bA `shouldSatisfy` (`covers` (fromList [1,2]))
      it "... covers [4]" $
        bA `shouldSatisfy` (`covers` (singleton 4))
      it "... does not cover [2,3,4]" $
        bA `shouldSatisfy` (\ac -> not (ac `covers` (fromList [2,3,4])))
    let cA = addProposition (fromList [3,4]) bA
    describe "singleton with added [1,2,3] and [3,4] ..." $ do 
      it "... is not total" $
        cA `shouldSatisfy` (not . (isTotal dom))
      it "... covers [1,2]" $
        cA `shouldSatisfy` (`covers` (fromList [1,2]))
      it "... covers [3,4]" $
        cA `shouldSatisfy` (`covers` (fromList [3,4]))
      it "... does not cover [2,3,4]" $
        cA `shouldSatisfy` (\ac -> not (ac `covers` (fromList [2,3,4])))
    let dA = addProposition (fromList [2,3]) cA
    describe "singleton with added [1,2,3], [3,4] and [2,3] ..." $ do 
      it "... is not total" $
        dA `shouldSatisfy` (not . (isTotal dom))
      it "... covers [2,3]" $
        dA `shouldSatisfy` (`covers` (fromList [2,3]))
      it "... covers [1,2]" $
        cA `shouldSatisfy` (`covers` (fromList [1,2]))
      it "... does not cover [2,3,4]" $
        cA `shouldSatisfy` (\ac -> not (ac `covers` (fromList [2,3,4])))
    let top = addProposition (fromList [1,2,3,4]) dA
    describe "singleton with added [1,2,3], [3,4], [1,3] and [1,2,3,4] ..." $ do 
      it "... covers [1,3,4]" $
        top `shouldSatisfy` (`covers` (fromList [1,3,4]))
      it "... is total" $
        top `shouldSatisfy` (isTotal dom)
  describe "is construction deterministic" $ do
    it "hamburger" $
      hamburger `shouldSatisfy` (isConstructionDeterministic lMapGraphI)
    it "strange3" $
      strange3 `shouldSatisfy` (isConstructionDeterministic lMapGraphI)
    it "slowLifting" $
      slowLifting `shouldSatisfy` (isConstructionDeterministic slowLiftingI)
    it "half celtic" $
      halfCeltic `shouldSatisfy` (isConstructionDeterministic lMapGraphI)
    it "allPaths" $
      allPaths`shouldSatisfy` (isConstructionDeterministic lMapGraphI)
    it "complicatedNeg" $
      complicatedNeg `shouldSatisfy` (isConstructionDeterministic lMapGraphI)
    it "slowFourConcise" $
      slowFourConcise `shouldSatisfy` (isConstructionDeterministic (conciseGraphI 4))
    it "slowLifting" $
      slowLifting `shouldSatisfy` (isConstructionDeterministic slowLiftingI)
  describe "is not construction deterministic" $ do
    it "dbg 1" $
      (dbg 1) `shouldSatisfy` (not . isConstructionDeterministic dbgI)
    it "dbg 3" $
      (dbg 3) `shouldSatisfy` (not . isConstructionDeterministic dbgI)
    it "force3d" $
      force3d `shouldSatisfy` (not . isConstructionDeterministic lMapGraphI)
    it "celtic" $
      celtic `shouldSatisfy` (not . isConstructionDeterministic lMapGraphI)
    it "complicatedPos" $
      complicatedPos `shouldSatisfy` (not . isConstructionDeterministic lMapGraphI)
    it "noPath" $
      noPath `shouldSatisfy` (not . isConstructionDeterministic noPathI)
