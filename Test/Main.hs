module Main (
  main
) where

import Test.Hspec

import qualified Test.AssocGraphSpec
import qualified Test.MapGraphSpec
import qualified Test.HomoSpec
import qualified Test.ArcConsSpec
--import qualified Test.UnlabeledBitGraphSpec
import qualified Test.DeterminismPropertySpec
--import qualified Test.CaleyGraphSpec
--import qualified Test.BitifySpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AssocGraph" Test.AssocGraphSpec.spec
  describe "MapGraph" Test.MapGraphSpec.spec
  describe "Homo" Test.HomoSpec.spec
  describe "ArcCons" Test.ArcConsSpec.spec
--  describe "UnlabeledBitGraph" Test.UnlabeledBitGraphSpec.spec
  describe "DeterminismProperty" Test.DeterminismPropertySpec.spec
--  describe "CaleyGraph" Test.CaleyGraphSpec.spec
--  describe "Bitify" Test.BitifySpec.spec

