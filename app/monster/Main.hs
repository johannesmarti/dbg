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
    list w = immediatelyGathers dt w
    forWord word = do
      putStr (prettyWord word)
      putStrLn ":"
      mapM_ (putStrLn . prettyWord) (list word)
  in mapM_ forWord wordList
