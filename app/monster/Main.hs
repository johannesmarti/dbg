module Main (
  main
) where

import Data.List (intersperse, intercalate)

import Descent
import Label
import Gathering
import Path

baseWord :: [Label]
baseWord = [Zero,Zero,Zero,One]
--baseWord = [Zero,Zero,One,One]
--baseWord = [Zero,Zero,One]
--baseWord = [Zero,One]

bound :: Int
bound = 256

main :: IO ()
main = let
    dt = descentTreeForBound bound
    ap = ascentPaths dt baseWord
    toOut = map (map (prettyReversePath prettyWord)) ap
    lins = intercalate ["", "===========",""] toOut
    linss = intersperse "" lins
    str = unlines linss
  in putStrLn str
