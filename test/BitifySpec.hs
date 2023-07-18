{-# LANGUAGE FlexibleInstances #-}
module BitifySpec (
   spec
) where

import qualified Data.Set as Set
import Test.Hspec

import Data.Label
import Graphs.BitGraph
import Bitify.Bitifier
import Graphs.LabeledGraphInterface
import Examples.Patterns
import Graphs.CommonLabeledGraphTypes


spec :: Spec
spec = do
  describe "bitify hamburger" $ do
    let bitification = genericBitifier hamburgerInterface hamburger
    let bitburger = labeledBitGraph bitification
    let iface = interface bitification
    it "0-sucs of a match [a,c]" $
      successors iface bitburger Zero 'a' `shouldBe` Set.fromList ['a','c']
    it "1-pred of a match [c]" $
      predecessors iface bitburger One 'a' `shouldBe` Set.fromList ['c']
    it "1-pred of b match [a,c,b]" $
      predecessors iface bitburger One 'b' `shouldBe` Set.fromList ['a','c','b']
  describe "bitify allPaths" $ do
    let bitification = genericBitifier labeledMapGraphInterface allPaths
    let bitpaths = labeledBitGraph bitification
    let pface = interface bitification
    it "bitpaths has 8 elements" $
      Set.size (domain pface bitpaths) `shouldBe` 8
