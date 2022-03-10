module Main (
  main
) where

import Test.Hspec

import qualified Test.AssocGraphSpec
import qualified Test.MapGraphSpec
import qualified Test.HomoSpec
import qualified Test.ArcConsSpec
import qualified Test.BitGraphSpec
import qualified Test.DeterminismPropertySpec
import qualified Test.CayleyGraphSpec
import qualified Test.BitifySpec
import qualified Test.LiftedSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AssocGraph" Test.AssocGraphSpec.spec
  describe "MapGraph" Test.MapGraphSpec.spec
  describe "Homo" Test.HomoSpec.spec
  describe "ArcCons" Test.ArcConsSpec.spec
  describe "BitGraph" Test.BitGraphSpec.spec
  describe "DeterminismProperty" Test.DeterminismPropertySpec.spec
  describe "CayleyGraph" Test.CayleyGraphSpec.spec
  describe "Bitify" Test.BitifySpec.spec
  describe "Lifted" Test.LiftedSpec.spec

