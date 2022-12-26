module Main (
  main
) where

import Descent
import Label
import Word

baseWord :: [Label]
baseWord = [Zero,One]

wordList :: [[Label]]
wordList = turns baseWord

bound :: Int
bound = 64

main :: IO ()
main = let
    dt = descentTreeForBound bound
    forWord word = let
        list = immediatelyGathers dt word
        rev = reverse word
        chainOf w = prettyWord w ++ workBack rev w where
          workBack [l] curr =
            case descentPredecessorMaybe dt l curr of
              Nothing -> ""
              Just n  -> " <" ++ labelToSymbol l ++ " " ++  prettyWord n
          workBack (l:rest) curr =
            case descentPredecessorMaybe dt l curr of
              Nothing -> ""
              Just n  -> " <" ++ labelToSymbol l ++ " " ++  prettyWord n ++ (workBack rest n)
      in do
           putStr (prettyWord word)
           putStrLn ":"
           mapM_ (putStrLn . chainOf) list
  in mapM_ forWord wordList
