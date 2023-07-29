module ConstructionGraphSpec (
   spec
) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Test.Hspec

import Data.Label
import Graphs.LabeledGraphInterface (domain)
import Conditions.ConstructionGraph
import Examples.Patterns

spec :: Spec
spec = do
  describe "weightMap" $ do
    let systemOfSpheres = [[1,2,3],[5,9],[4,8]]
    let distances = [(1,0),(2,0),(3,0),(5,1),(9,1),(4,2),(8,2)]
    let realSys = map Set.fromList systemOfSpheres
    it "weightMap [[1,2,3],[5,9],[4,8]] == [(1,0),(2,0),(3,0),(5,1),(9,1),(4,2),(8,2)]" $
      weightMap realSys `shouldBe` Map.fromList distances
  describe "powerWeightMap" $ do
    let force3dWeight = powerWeightMap force3dInterface force3d
    it "weight {a} = 0" $
      force3dWeight (Set.singleton 'a') `shouldBe` 0
    it "weight {c} = 0" $
      force3dWeight (Set.singleton 'c') `shouldBe` 0
    let sab = Set.fromList ['a','b']
    it "weight {a,b} = 1" $
      force3dWeight sab `shouldBe` 1
    let sac = Set.fromList ['a','c']
    it "weight {a,c} = 2" $
      force3dWeight sac `shouldBe` 2
    let scb = Set.fromList ['c','b']
    it "weight {c,b} = 3" $
      force3dWeight scb `shouldBe` 3
    let sacb = Set.fromList ['a','c','b']
    it "weight {a,c,b} = 3" $
      force3dWeight sacb `shouldBe` 3
  describe "bestPowerPredecessors on big5" $ do
    let big5Weight = powerWeightMap big5Interface big5
    let bppBig5 = bestPowerPredecessors big5Interface big5 big5Weight
    let s134 = Set.fromList [1,3,4]
    it "of {1,3,4} under Zero is {0,2,3}" $
      bppBig5 Zero s134 `shouldBe` [Set.fromList [0,2,3]]
    it "of {1,3,4} under One is {3,4}" $
      bppBig5 One s134 `shouldBe` [Set.fromList [3,4]]
    let s023 = Set.fromList [0,2,3]
    it "of {0,2,3} under Zero is {4}" $
      bppBig5 Zero s023 `shouldBe` [Set.singleton 4]
    it "of {0,2,3} under One is {0,1}" $
      bppBig5 One s023 `shouldBe` [Set.fromList [0,1]]
    let dom = domain big5Interface big5
    it "of {0,1,2,3,4} under Zero is {0,2,3}" $
      bppBig5 Zero dom `shouldBe` [Set.fromList [0,2,3]]
    it "of {0,1,2,3,4} under One is {0,1,3,4}" $
      bppBig5 One dom `shouldBe` [Set.fromList [0,1,3,4]]
  innerSpec
