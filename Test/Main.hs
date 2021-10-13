module Main (
  main
) where

import Test.Hspec

import qualified Test.AssocGraphSpec
import qualified Test.MapGraphSpec
import qualified Test.DeBruijnSpec
import qualified Test.HomoSpec
import qualified Test.ArcConsSpec
import qualified Test.UnlabeledBitGraphSpec
import qualified Test.DeterminismPropertySpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AssocGraph" Test.AssocGraphSpec.spec
  describe "MapGraph" Test.MapGraphSpec.spec
  describe "DeBruijn" Test.DeBruijnSpec.spec
  describe "Homo" Test.HomoSpec.spec
  describe "ArcCons" Test.ArcConsSpec.spec
  describe "UnlabeledBitGraph" Test.UnlabeledBitGraphSpec.spec
  describe "DeterminismProperty" Test.DeterminismPropertySpec.spec

