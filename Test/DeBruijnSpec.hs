module Test.DeBruijnSpec (
   spec
) where

import Test.Hspec

import DeBruijn
import Graph

spec :: Spec
spec = do
    it "dbg 3 is coherent" $
      wellDefined dbgI (dbg 3)
