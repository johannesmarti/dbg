module SpiralSpec (
   spec
) where

import Data.Set as S
import Data.Vector as V
import Test.Hspec

import Data.Label
import Plans.Spiral
import Examples.Patterns

s001 :: Spiral Char
s001 = fromHub force3dInterface force3d [Zero,Zero,One] ['a','c','b']

spec :: Spec
spec = do
  describe "generatedSubspirals in force 3d for 001 at [a,c,b]" $ do
    let gen = generatedSubspirals force3dInterface force3d s001
    let dom = domain force3DInterface force3d
    it "generate completely from [{},{b},{c}]" $
      gen (V.fromList [S.empty, S.singleton 'b', S.singleton 'c'])
        `shouldBe` [V.fromList [dom,dom,dom]]
