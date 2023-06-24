module PlanSpec (
   spec
) where

import Test.Hspec

import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import qualified Data.Vector as V

import Patterns
import Plan
import Label
import LabeledGraph
import LiftedGraph

force3dPlan :: Plan Char
force3dPlan =
  insert [Zero] (spoke 'a' []) $
  insert [One] (spoke 'b' []) $
  insert [Zero,One] (spoke 'a' [('b', 1)]) $
  insert [One,Zero] (spoke 'c' []) $
  insert [Zero,One,One] (spoke 'b' []) $
  insert [Zero,One,One,Zero] (spoke 'c' []) $
  insert [Zero,One,One,Zero,One] (spoke 'a' []) $
  insert [One,Zero,Zero] (spoke 'a' []) $
  insert [One,Zero,Zero,One] (spoke 'b' []) $
  insert [One,Zero,Zero,One,Zero] (spoke 'c' []) $ empty

spec :: Spec
spec = do
  describe "wrapSpiral on complete spiral 001 in force3d" $ do
    let s001 = spoke 'a' [('b', 1), ('c', 2)]
    let s010 = spoke 'c' [('a', 1), ('b', 3)]
    let s100 = spoke 'b' [('a', 1), ('c', 2)]
    let spokes = V.fromList [s001, s010, s100]
    let lg = LiftedGraph.fromLGraph force3dI force3d
    let (fatVec, extendedLg) = runState (wrapSpiral spokes) lg
    let ig = graph extendedLg
    let f001 = fatVec V.! 0
    let f010 = fatVec V.! 1
    let f100 = fatVec V.! 2
    let ha = hasArc intGraphI ig
    it "fat 001 is in domain" $
      f001 `elem` domain intGraphI ig `shouldBe` True
    it "fat 001 0-> fat 010 0-> fat 100 1-> fat 001" $
      ha Zero (f001, f010) && ha Zero (f010, f100) && ha One (f100, f001) `shouldBe` True
