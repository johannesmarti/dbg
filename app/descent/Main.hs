module Main (
  main
) where

import Descent
import Label
import Word

forWord :: DescentTree -> [Label] -> IO ()
forWord dt word = let
    descentStatus = lookupWord word dt
    descentInfoString = prettyDescentInfo word descentStatus
    gatherInit = gathers dt word
    gatherString = prettyWord gatherInit ++ "*"
    ascentsTo = ascentNode dt word
  in putStrLn $ (prettyWord word) ++ ": " ++ descentInfoString ++
                  (if not (canDescent descentStatus) then []
                   else " gathers " ++ gatherString ++
                        " ascents to " ++ prettyWord ascentsTo) ++
                  " gathered by " ++ prettyWord (immediatelyGatheredBy dt word)

main :: IO ()
main = let
    dt = descentTreeForBound 1000
    toShow = take 1024 . tail $ Word.allWords labelsList
  in mapM_ (forWord dt) toShow
