module ListWordSpec (
   spec
) where

import Test.Hspec

import Data.ListWord


turnList :: (Show a, Eq a) => [a] -> Spec
turnList l =
  describe ((show l) ++ " turns nicely") $ do
    let ts = realTurns l
    let turnCheck f g = head (f ts)`shouldBe` g l
    it "turning forward" $ turnCheck turnForward turnForward
    it "turning backward" $ turnCheck turnBackward turnBackward
    it "turning 5 times backward" $
      turnCheck (turnBackward . turnBackward . turnBackward . turnBackward . turnBackward)
                (turnBackward . turnBackward . turnBackward . turnBackward . turnBackward)
    it "turning backward and forward" $
      turnCheck (turnForward . turnForward . turnBackward . turnForward . turnBackward)
                turnForward
    

spec :: Spec
spec = do
  describe "turning works nicely" $ do
    turnList [1]
    turnList [1,2,3,4,5]
    turnList ['a','b','a','b']
