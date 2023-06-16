module Main (
  main
) where

import System.Environment

import CoveringGraph
import Label

printer :: CoveringNode -> String
printer node = 
  (prettyWord (turningWord node) ++ " at address "
            ++ prettyWord (address node)) ++ " with parent "
            ++ show (turningWord (parent node))

listPrinter :: [CoveringNode] -> IO ()
listPrinter [] = putStrLn "===="
listPrinter (a:as) = do
  putStrLn "===="
  -- mapM_ putStrLn (addressPrinter a)
  putStrLn (addressPrinter a)
  listPrinter as

childrenOfNode :: (CoveringNode -> Bool) -> CoveringNode -> IO ()
childrenOfNode predicte n = do
  putStrLn "The node:"
  printer n
  putStrLn "hasChildren:"
  listPrinter (children predicate n)

node :: CoveringNode
--node = lookupAddress [Zero,One]
node = zero

main :: IO ()
main = do
  args <- getArgs
  let bound = read (head args)
  childrenOfNode (\g -> length (turningWord g) < bound) node
