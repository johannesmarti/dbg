module Test.DeterminismPropertySpec (
   spec
) where

import Data.Maybe
import Test.Hspec

import DeBruijn
import Graph
import Patterns
import MapGraph
import DeterminismProperty

spec :: Spec
spec = do
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
