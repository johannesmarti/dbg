module Main (
  main
) where

import System.Environment

import ArcCons
import DeBruijn
import Homo
import Search

import Patterns

dg = deBruijnGraph

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  --putStrLn (show (arcConsHomos (dg n) (testPattern)))
  putStrLn (show (searchForDbgHomo n (allPaths)))
