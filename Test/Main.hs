module Main (
  main
) where

import Test.Hspec

import qualified Test.AssocGraphSpec
import qualified Test.MapGraphSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AssocGraph" Test.AssocGraphSpec.spec
  describe "MapGraph" Test.MapGraphSpec.spec

