module Descent (
  prettyDescentStatus,
  lookupWord,
  descentTreeForBound,
) where

import Control.Exception
import Data.Maybe
import Debug.Trace

import Label
import Word
import WordTree

data DescentStatus = Divisible | Cycle | Descent [Label]
  deriving Show

prettyDescentStatus :: DescentStatus -> String
prettyDescentStatus Divisible = "divisible"
prettyDescentStatus Cycle = "cycle"
prettyDescentStatus (Descent to) = "descent to " ++ prettyWord to

maybeDescent :: DescentStatus -> Maybe [Label]
maybeDescent (Descent word) = Just word
maybeDescent _              = Nothing

type DescentTree = WordTree (Maybe DescentStatus)

emptyDescentTree :: DescentTree
emptyDescentTree = wordTreeFromFunction fct where
  fct word = if isDivisible word
                then Just Divisible
                else Nothing

baseDescentTree :: DescentTree
baseDescentTree = dt2 where
  dt1 = insertNode [One] Cycle (insertNode [Zero] Cycle emptyDescentTree)
  dt2 = addAscentOfWord [One] (addAscentOfWord [Zero] dt1)

lookupWord :: [Label] -> DescentTree -> DescentStatus
lookupWord word dt =
  fromMaybe (error $ "trying to lookup word (" ++ prettyWord word ++ ") that is not yet in descent tree.")
            (labelOfWord dt word)

insertNode :: [Label] -> DescentStatus -> DescentTree -> DescentTree
insertNode word status dt = {-trace ("insert " ++ show status ++ " at " ++ prettyWord word) $-} updateWord updater word dt where
  updater Nothing = Just status
  updater (Just _) = error "trying to insert word in descent tree that already has an entry"

descentSuccessor :: [Label] -> DescentTree -> [Label]
descentSuccessor word dt = case lookupWord word dt of
  Divisible -> error "trying to descent from divisible"
  Cycle     -> turnForward word
  Descent w -> w

descentPathTo :: Label -> [Label] -> DescentTree -> [Label]
descentPathTo target toWalkFrom dt =
  if (toWalkFrom == [Zero,One] && target == One)
    then [Zero]
  else if (toWalkFrom == [One,Zero] && target == Zero)
   then [One]
  else if (toWalkFrom == [Zero] && target == Zero)
   then []
  else if (toWalkFrom == [Zero] && target == One)
   then [Zero]
  else if (toWalkFrom == [One] && target == One)
   then []
  else if (toWalkFrom == [One] && target == Zero)
   then [One]
  else head toWalkFrom :
          descentPathTo target (descentSuccessor toWalkFrom dt) dt

addCycleOfWord :: [Label] -> DescentTree -> DescentTree
addCycleOfWord word dt = let
    wordsOnCycle = turns word
    descentInformation word = (labelOfWord dt word) >>= maybeDescent
    descentPoints = filter (isJust .  descentInformation) wordsOnCycle
  in assert (not $ isDivisible word) $
     if null descentPoints
       then trace ("no descent points for " ++ prettyWord word) dt
       else {-trace (show descentPoints ++ "\n") $-} foldl (flip addPredecessorsOnCycle) dt descentPoints

addAscentOfWord :: [Label] -> DescentTree -> DescentTree
addAscentOfWord word dt = dt' where
  firstLetter = case last word of Zero -> One
                                  One  -> Zero
  ascentPath = descentPathTo firstLetter word dt
  newNode = firstLetter : ascentPath
  dt' = insertNode newNode (Descent word) dt

addPredecessorsOnCycle :: [Label] -> DescentTree -> DescentTree
addPredecessorsOnCycle word dt = workOnPredecessors word dt' where
  dt' = addAscentOfWord word dt
  workOnPredecessors w d = let
      predecessor = turnBackward w
      d' = insertNode predecessor Cycle d
      d'' = addAscentOfWord predecessor d'
    in case (labelOfWord dt predecessor) >>= maybeDescent of
         Just _  -> d
         Nothing -> workOnPredecessors predecessor d''

descentTreeForBound :: Int -> DescentTree
descentTreeForBound bound =
  foldl (flip addCycleOfWord) baseDescentTree nfs where
    nfs = take bound . filter isBaseWord . tail . tail . tail $ Word.allWords labelsList
  
