module Main (
  main
) where

import Debug.Trace

import Descent
import Label
import Word


main :: IO ()
main = let
    dt = descentTreeForBound 600
    words = take 512 . tail $ Word.allWords labelsList
    forWord w = putStrLn $ (prettyWord w) ++ ": " ++ (prettyDescentStatus (lookupWord w dt))
  in mapM_ forWord words
