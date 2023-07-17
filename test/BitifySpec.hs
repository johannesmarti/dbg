{-# LANGUAGE FlexibleInstances #-}
module BitifySpec (
   spec
) where

import qualified Data.Set as Set
import Test.Hspec

import Data.Label
import Graphs.BitGraph
import Bitify
import Graphs.LabeledGraphInterface
import Examples.Patterns
import Graphs.CommonLabeledGraphTypes
import LabeledWrappedGraph


instance Show (LabeledWrappedGraph LabeledBitGraph Node y) where
  show g = "fuck type classes"

spec :: Spec
spec = do
  describe "bitify hamburger" $ do
    let (bitburger, bsize) = labeledBitify hamburgerInterface hamburger
    let iface = labeledWrappedGraphInterface (labeledBitGraphInterface bsize)
    it "0-sucs of a match [a,c]" $
      successors iface bitburger Zero 'a' `shouldBe` Set.fromList ['a','c']
    it "1-pred of a match [c]" $
      predecessors iface bitburger One 'a' `shouldBe` Set.fromList ['c']
    it "1-pred of b match [a,c,b]" $
      predecessors iface bitburger One 'b' `shouldBe` Set.fromList ['a','c','b']
  describe "bitify allPaths" $ do
    let (bitpaths, psize) = labeledBitify labeledMapGraphInterface allPaths
    let pface = labeledWrappedGraphInterface (labeledBitGraphInterface psize)
    it "bitpaths has 8 elements" $
      Set.size (domain pface bitpaths) `shouldBe` 8
