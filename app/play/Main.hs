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
            ++ prettyWord (address node)) ++ " is ascending: "
            ++ show (isAscending node)

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
  let nfs = take numNodes cycles
  let cycs = map cycleOfNode nfs
  let isInteresting cycle = length (filter isAscending cycle) > 2
  let wow = filter isInteresting cycs
  putStrLn (show (length wow))
  listPrinter (head wow)
