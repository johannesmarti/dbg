module Main (
  main
) where

import System.Environment

import DeBruijn
import Homo

import Patterns

dg = deBruijnGraph

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  putStrLn (show (searchHomos (dg n) (triple)))
