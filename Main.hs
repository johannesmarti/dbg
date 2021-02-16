module Main (
  main
) where

import System.Environment

import ArcCons
import DeBruijn
import Homo

import Patterns

dg = deBruijnGraph

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  putStrLn (show (arcConsHomos (dg n) (triple)))
  --putStrLn (show (searchHomos (dg n) (triple)))
