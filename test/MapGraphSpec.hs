module MapGraphSpec (
   spec
) where

import Data.Set as Set
import Test.Hspec

import Data.Label
import Graphs.CommonLabeledGraphTypes
import Graphs.LabeledGraphInterface
import Graphs.MapGraph
import qualified Graphs.GraphInterface as GI
import Graphs.PairGraph

f1 Zero = [(1,2),(1,1),(2,5)]
f1 One = [(5,5)]

g1 :: LabeledMapGraph Int
g1 = mapFromFunction f1

spec :: Spec
spec = do
  describe "simple graph g1" $ do
    it "0-succs of 1 match [1,2]" $
      successors labeledMapGraphInterface g1 Zero 1 `shouldBe` Set.fromList [1,2]
    it "0-pred of 5 match [2]" $
      predecessors labeledMapGraphInterface g1 Zero 5 `shouldBe` Set.fromList [2]
  describe "inserting (4,5) into {(5,5)}" $ do
    let graph = graphOfLabel g1 One
    let newGraph = addArc (addNode graph 4) (4,5)
    it "succs of 4 match [5]" $
      GI.successors mapGraphInterface newGraph 4 `shouldBe` Set.fromList [5]
    it "preds of 5 match [4,5]" $
      GI.predecessors mapGraphInterface newGraph 5 `shouldBe` Set.fromList [4,5]
