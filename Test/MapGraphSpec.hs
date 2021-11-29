module Test.MapGraphSpec (
   spec
) where

import Data.Set as Set
import Test.Hspec

import CommonLGraphTypes
import LabeledGraph
import MapGraph
import qualified Graph
import PairGraph

f1 Zero = [(1,2),(1,1),(2,5)]
f1 One = [(5,5)]

g1 :: LMapGraph Int
g1 = mapFromFunction f1

spec :: Spec
spec = do
  describe "simple graph g1" $ do
    it "0-succs of 1 match [1,2]" $
      successors lMapGraphI g1 Zero 1 `shouldBe` Set.fromList [1,2]
    it "0-pred of 5 match [2]" $
      predecessors lMapGraphI g1 Zero 5 `shouldBe` Set.fromList [2]
  describe "inserting (4,5) into {(5,5)}" $ do
    let graph = graphOfLabel g1 One
    let newGraph = addArc (addNode graph 4) (4,5)
    it "succs of 4 match [5]" $
      Graph.successors mapGraphI newGraph 4 `shouldBe` Set.fromList [5]
    it "preds of 5 match [4,5]" $
      Graph.predecessors mapGraphI newGraph 5 `shouldBe` Set.fromList [4,5]
