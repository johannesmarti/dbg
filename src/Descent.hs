module Descent (

) where

import Control.Exception
import Data.Maybe

import Label
import Word
import WordTree

data DescentStatus = Divisible | Cycle | Descent [Label]

maybeDescent :: DescentStatus -> Maybe [Label]
maybeDescent (Descent word) = Just word
maybeDescent _              = Nothing

type DescentTree = WordTree (Maybe DescentStatus)

emptyDescentTree :: DescentTree
emptyDescentTree = wordTreeFromFunction fct where
  fct word = if isDivisible word
                then Just Divisible
                else Nothing

lookupWord :: [Label] -> DescentTree -> DescentStatus
lookupWord word dt =
  fromMaybe (error "trying to lookup word that is not yet in descent tree.")
            (labelOfWord dt word)

descentSuccessor :: [Label] -> DescentTree -> [Label]
descentSuccessor word dt = case lookupWord word dt of
  Divisible -> error "trying to descent from divisible"
  Cycle     -> turnForward word
  Descent w -> w

addCycleOfWord :: [Label] -> DescentTree -> DescentTree
addCycleOfWord word dt = let
    wordsOnCycle = turns word
    descentInformation word = (labelOfWord dt word) >>= maybeDescent
    descentPoints = catMaybes $ map descentInformation wordsOnCycle
  in assert (not $ isDivisible word) $ undefined

