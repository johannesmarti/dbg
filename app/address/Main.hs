module Main (
  main
) where

import CoveringGraph
import Label
import Word

a :: [Label]
a = [One,Zero,Zero,Zero,One,Zero,Zero,One]
--a = [Zero,One,One,One]
--b = a ++ [Zero,One,Zero,Zero,Zero] ++ a ++ [Zero,One,One]

addressList :: [[Label]]
addressList = map ([One,Zero,Zero,One,Zero,One,Zero] ++) [
  [],
  [Zero] ]

list :: [CoveringNode]
list = map lookupAddress addressList

addressPrinter :: CoveringNode -> [String]
addressPrinter node = 
  if node == epsilon then []
  else (prettyWord (turningWord node) ++ " at address "
            ++ prettyWord (address node)) : addressPrinter (parent node)

listPrinter :: [CoveringNode] -> IO ()
listPrinter [] = putStrLn "===="
listPrinter (a:as) = do
  putStrLn "===="
  mapM_ putStrLn (addressPrinter a)
  listPrinter as

main :: IO ()
main = let
    p0 = predecessor Zero
    p1 = predecessor One
    --node = p0 . p1 . p0 . p0 . p1 . p0 . p1 . p0 . p0 $ one
  --in print addressList -- listPrinter list
  in listPrinter list
