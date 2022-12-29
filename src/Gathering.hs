module Gathering (
  generateAscentPath,
  longer,
  ascentPaths,
  --verifyPath,
) where

import Descent
import Label
import Path
import Word

nodeOnPath :: Path [Label] -> [Label] -> Bool
nodeOnPath (There a) b = a == b
nodeOnPath (Step a _ cont) b =
  if length a > length b
    then False
  else if a == b
    then True
  else nodeOnPath cont b

longer :: Path [Label] -> Path [Label] -> Bool
longer a b = nodeOnPath a (start b)

generateAscentPathWorker :: DescentTree -> [Label] -> [Label] -> Path [Label]
generateAscentPathWorker _ [] word = There word
generateAscentPathWorker dt (next:rest) word =
  case descentPredecessorMaybe dt next word of
    Nothing -> There word
    Just x  -> Step word next (generateAscentPathWorker dt rest x)

generateAscentPath :: DescentTree -> [Label] -> [Label] -> Path [Label]
generateAscentPath dt along start =
  generateAscentPathWorker dt (cycle (reverse along)) start

populateWordLists :: DescentTree -> [Label] -> [[[Label]]]
populateWordLists dt word = map (immediatelyGathers dt) (realTurns word)

ascentPaths :: DescentTree -> [Label] -> [[Path [Label]]]
ascentPaths dt baseWord = let
    wordLists = populateWordLists dt baseWord
    makePaths w wl = map (generateAscentPath dt w) (head wl)
    allPaths = onAllTurns makePaths baseWord wordLists
  in filterPaths allPaths

filterPaths :: [[Path [Label]]] -> [[Path [Label]]]
filterPaths pss = map (filter undominated) pss where
  ps = concat pss
  undominated p = all (nd p) ps
  nd p c = not (c `longer` p) || (p `longer` c) 


-- The following is not really working well. Somehow the descent tree is just not developed far enough
verifyPath :: DescentTree -> [Label] -> Path [Label] -> Bool
verifyPath dt word (There x) = word == immediatelyGatheredBy dt x
verifyPath dt word (Step x _ cont) = word == immediatelyGatheredBy dt x &&
  verifyPath dt (turnBackward word) cont
