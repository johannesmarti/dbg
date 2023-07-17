module PlanSpec (
   spec
) where

import Test.Hspec

import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import qualified Data.Vector as V

import Examples.Patterns
import Examples.Plans
import Plan
import ExecutePlan
import Data.Label
import Graphs.LabeledGraphInterface
import LiftedGraph

spec :: Spec
spec = do
  describe "wrapSpiral on complete spiral 001 in force3d" $ do
    let s001 = spoke 'a' [('b', 1), ('c', 2)]
    let s010 = spoke 'c' [('a', 1), ('b', 3)]
    let s100 = spoke 'b' [('a', 1), ('c', 2)]
    let spokes = V.fromList [s001, s010, s100]
    let lg = LiftedGraph.fromLabeledGraph force3dInterface force3d
    let (fatVec, extendedLg) = runState (wrapSpiral spokes) lg
    let ig = graph extendedLg
    let f001 = fatVec V.! 0
    let f010 = fatVec V.! 1
    let f100 = fatVec V.! 2
    let ha = hasArc intGraphInterface ig
    it "fat 001 is in domain" $
      f001 `elem` domain intGraphInterface ig `shouldBe` True
    it "fat 001 0-> fat 010 0-> fat 100 1-> fat 001" $
      ha Zero (f001, f010) && ha Zero (f010, f100) && ha One (f100, f001) `shouldBe` True
  let (lg,dsl) = executePlan force3dInterface force3d force3dPlan
  let ig = graph lg
  describe "executing plan on force3d" $ do
    it "is generating double self loop" $
      (hasArc intGraphInterface ig Zero (dsl,dsl) &&
       hasArc intGraphInterface ig One (dsl,dsl)) `shouldBe` True
  let (lg,dsl) = executePlan alloc3Interface alloc3 alloc3Plan
  let ig = graph lg
  describe "executing plan on alloc3" $ do
    it "is generating double self loop" $
      (hasArc intGraphInterface ig Zero (dsl,dsl) &&
       hasArc intGraphInterface ig One (dsl,dsl)) `shouldBe` True
