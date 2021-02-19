module Main (
  main
) where

import Test.Hspec

import qualified Test.ArcConsSpec
import qualified Test.AssocGraphSpec
import qualified Test.MapGraphSpec
import qualified Test.HomoSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AssocGraph" Test.AssocGraphSpec.spec
  describe "MapGraph" Test.MapGraphSpec.spec
  describe "Homo" Test.HomoSpec.spec
  describe "ArcCons" Test.ArcConsSpec.spec

