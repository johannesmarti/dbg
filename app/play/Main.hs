module Main (
  main
) where

import System.Environment

import CoveringGraph
import Label
import Word
import Path (labelList)

addressPrinter :: CoveringNode -> String
addressPrinter node = 
  (prettyWord (turningWord node) ++ " at address "
            ++ prettyWord (address node)) ++ " with path down "
            ++ prettyWord (reverse . labelList . fullPathDown $ node)

listPrinter :: [CoveringNode] -> IO ()
listPrinter [] = putStrLn "===="
listPrinter (a:as) = do
  putStrLn "===="
  -- mapM_ putStrLn (addressPrinter a)
  putStrLn (addressPrinter a)
  listPrinter as

main :: IO ()
main = do
  args <- getArgs
  let numNodes = read (head args)
  listPrinter (take numNodes (tail generateNodes))
