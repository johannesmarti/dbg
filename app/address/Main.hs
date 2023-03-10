module Main (
  main
) where

import CoveringGraph
import Label
import Word

a :: [Label]
a = [Zero,One,One,One]
b = a ++ [Zero,One,Zero,Zero,Zero] ++ a ++ [Zero,One,One]

addressPrinter :: CoveringNode -> [String]
addressPrinter node = 
  if node == epsilon then []
  else (prettyWord (turningWord node) ++ " at address "
            ++ prettyWord (address node)) : addressPrinter (parent node)

main :: IO ()
main = let
    p0 = predecessor Zero
    p1 = predecessor One
    --node = p0 . p1 . p0 . p0 . p1 . p0 . p1 . p0 . p0 $ one
    node = lookupAddress b
  in mapM_ putStrLn (addressPrinter node)
