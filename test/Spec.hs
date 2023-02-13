module Main (
  main
) where

import Test.Hspec

import qualified AssocGraphSpec
import qualified MapGraphSpec
import qualified ConciseGraphSpec
import qualified HomomorphismSpec
import qualified ArcConsSpec
import qualified BitGraphSpec
import qualified DeterminismPropertySpec
import qualified CayleyGraphSpec
import qualified BitifySpec
import qualified LiftedSpec
import qualified WordSpec
import qualified GatheringSpec
import qualified FlyingPigSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AssocGraph" AssocGraphSpec.spec
  describe "MapGraph" MapGraphSpec.spec
  describe "ConciseGraph" ConciseGraphSpec.spec
  describe "Homomorphism" HomomorphismSpec.spec
  describe "ArcCons" ArcConsSpec.spec
  describe "BitGraph" BitGraphSpec.spec
  describe "DeterminismProperty" DeterminismPropertySpec.spec
  describe "CayleyGraph" CayleyGraphSpec.spec
  describe "Bitify" BitifySpec.spec
  describe "Lifted" LiftedSpec.spec
  describe "Word" WordSpec.spec
  describe "Gathring" GatheringSpec.spec
  describe "FlyingPig" FlyingPigSpec.spec

