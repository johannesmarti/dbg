module Main (
  main
) where

import System.Environment

import DeBruijn
import Homo

dg = deBruijnGraph

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  putStrLn (show (searchHomos (dg n) (dg 2)))
