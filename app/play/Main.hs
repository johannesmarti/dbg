module Main (
  main
) where

import qualified TurningVector as TV

main :: IO ()
main = do
  let v1 = TV.fromList [1,2,3,4,5,6,7]
  let v2 = TV.turnBackward . TV.turnBackward $ v1
  print $ TV.toList (TV.zipWithList (+) v2 [1,2,3]) -- `shouldBe` [7,9,4,2,3,4,5]
  print $ TV.toList (TV.zipWithList (+) (TV.fromList [1,2]) [1,2,3,4,5]) -- `shouldBe` [10,8]
