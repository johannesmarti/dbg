module Test.BitifySpec (
   spec
) where

import qualified Data.Set as Set
import Test.Hspec

import Bitify
import Graph
import MapGraph
import Patterns
import WrappedGraph


spec :: Spec
spec = do
  describe "bitify hamburger" $ do
    let bitburger = bitify mapGraphI hamburger
    it "0-sucs of a match [a,c]" $
      successors wrappedGraphI bitburger Zero 'a' `shouldBe` Set.fromList ['a','c']
    it "1-pred of a match [c]" $
      predecessors wrappedGraphI bitburger One 'a' `shouldBe` Set.fromList ['c']
    it "1-pred of b match [a,c,b]" $
      predecessors wrappedGraphI bitburger One 'b' `shouldBe` Set.fromList ['a','c','b']
