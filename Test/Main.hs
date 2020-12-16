module Main (
  main
) where

import Test.Hspec

import qualified Test.AssocGraphSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AssocGraph" Test.AssocGraphSpec.spec

