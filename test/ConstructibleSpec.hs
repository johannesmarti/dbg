module ConstructibleSpec (
   spec
) where

import Data.Maybe
import Data.Set
import Test.Hspec

import Graphs.DeBruijnGraph
import Graphs.LabeledGraphInterface
import Graphs.ConciseGraph
import Examples.Patterns
import Graphs.CommonLabeledGraphTypes
import Conditions.Constructible


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
      strictDet `shouldSatisfy` (not . isStrictlyConstructible labeledMapGraphInterface)
  describe "is strictly constructible" $ do
    it "dbg 3" $
      (dbg 3) `shouldSatisfy` (isStrictlyConstructible dbgInterface)
    it "force3d" $
      force3d `shouldSatisfy` (isStrictlyConstructible labeledMapGraphInterface)
    it "hamburger" $
      hamburger `shouldSatisfy` (isStrictlyConstructible labeledMapGraphInterface)
    it "strange3" $
      strange3 `shouldSatisfy` (isStrictlyConstructible labeledMapGraphInterface)
    it "4003476 of size 4" $
      4003476 `shouldSatisfy` (isStrictlyConstructible (conciseGraphInterface 4))
  describe "is not strongly constructible " $ do
    it "hamburger" $
      hamburger `shouldSatisfy` (not . isStronglyConstructible labeledMapGraphInterface)
    it "strange3" $
      strange3 `shouldSatisfy` (not . isStronglyConstructible labeledMapGraphInterface)
  describe "is strongly constructible" $ do
    it "dbg 1" $
      (dbg 1) `shouldSatisfy` (isStronglyConstructible dbgInterface)
    it "dbg 3" $
      (dbg 3) `shouldSatisfy` (isStronglyConstructible dbgInterface)
    it "force3d" $
      force3d `shouldSatisfy` (isStronglyConstructible labeledMapGraphInterface)
    it "slowFourConcise" $
      slowFourConcise `shouldSatisfy` (isStronglyConstructible (conciseGraphInterface 4))
    it "slowLifting" $
      slowLifting `shouldSatisfy` (isStronglyConstructible slowLiftingInterface)
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
  describe "is not constructible" $ do
    it "hamburger" $
      hamburger `shouldSatisfy` (not . isConstructible labeledMapGraphInterface)
    it "strange3" $
      strange3 `shouldSatisfy` (not . isConstructible labeledMapGraphInterface)
    it "slowLifting"  $
      slowLifting `shouldSatisfy` (not . isConstructible slowLiftingInterface)
    it "half celtic" $
      halfCeltic `shouldSatisfy` (not . isConstructible labeledMapGraphInterface)
    it "allPaths" $
      allPaths`shouldSatisfy` (not . isConstructible labeledMapGraphInterface)
    it "complicatedNeg" $
      complicatedNeg `shouldSatisfy` (not . isConstructible labeledMapGraphInterface)
    it "slowFourConcise" $
      slowFourConcise `shouldSatisfy` (not . isConstructible (conciseGraphInterface 4))
    it "slowLifting" $
      slowLifting `shouldSatisfy` (not . isConstructible slowLiftingInterface)
  describe "is constructible" $ do
    it "dbg 1" $
      (dbg 1) `shouldSatisfy` (isConstructible dbgInterface)
    it "dbg 3" $
      (dbg 3) `shouldSatisfy` (isConstructible dbgInterface)
    it "force3d" $
      force3d `shouldSatisfy` (isConstructible labeledMapGraphInterface)
    it "celtic" $
      celtic `shouldSatisfy` (isConstructible labeledMapGraphInterface)
    it "complicatedPos" $
      complicatedPos `shouldSatisfy` (isConstructible labeledMapGraphInterface)
    it "noPath" $
      noPath `shouldSatisfy` (isConstructible noPathInterface)
