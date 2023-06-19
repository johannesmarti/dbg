module Main (
  main
) where

import System.Environment
import Data.Maybe (listToMaybe)

import CoveringGraph
import Label
import Word
import Path (labelList)

addressPrinter :: CoveringNode -> String
addressPrinter node = 
  (prettyWord (turningWord node) ++ " at address "
            ++ prettyWord (address node)) ++ " is ascending: "
            ++ show (isDescending node)

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
  let isInteresting cycle = length (filter isDescending cycle) > 2
  let isVeryInteresting cycle = length (filter isDescending cycle) > 3
  let isIncredibelyInteresting cycle = length (filter isDescending cycle) > 4
  let wow = filter isInteresting cycs
  --let wow = filter isVeryInteresting cycs
  --let wow = filter isIncredibelyInteresting cycs
  case listToMaybe wow of
    Nothing -> putStrLn "nothing"
    Just c -> listPrinter c
  putStrLn (show (length wow))
