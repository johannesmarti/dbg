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
            ++ prettyWord (turningWord (parent node))

listPrinter :: [CoveringNode] -> IO ()
listPrinter [] = putStrLn "===="
listPrinter (a:as) = do
  putStrLn "===="
  -- mapM_ putStrLn (addressPrinter a)
  putStrLn (printer a)
  listPrinter as

childrenOfNode :: (CoveringNode -> Bool) -> CoveringNode -> IO ()
childrenOfNode predicate n = do
  putStrLn "The node:"
  putStrLn (printer n)
  putStrLn "hasChildren:"
  mapM_ listPrinter (childrenCycles predicate n)

node :: CoveringNode
--node = lookupAddress [Zero,One]
node = predecessor One zero

main :: IO ()
main = do
  args <- getArgs
  let bound = read (head args)
  childrenOfNode (\g -> length (turningWord g) < bound) node
