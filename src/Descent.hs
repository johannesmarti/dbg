module Descent (
  canDescent,
  prettyDescentStatus,
  prettyDescentInfo,
  DescentTree,
  lookupWord,
  ascentNode,
  descentSuccessor,
  descentTreeForBound,
  gathers,
  immediatelyGatheredBy,
  immediatelyGathers,
) where

import Control.Exception
import Data.List (find,inits,isPrefixOf)
import Data.Maybe
--import Debug.Trace

import Label
import Word
import WordTree

data DescentStatus = Divisible | Isolated | Cycle | Descent [Label]
  deriving Show

canDescent :: DescentStatus -> Bool
canDescent Divisible   = False
canDescent Isolated    = False
canDescent Cycle       = True
canDescent (Descent _) = True

maybeDescent :: DescentStatus -> Maybe [Label]
maybeDescent (Descent word) = Just word
maybeDescent _              = Nothing

prettyDescentStatus :: DescentStatus -> String
prettyDescentStatus Divisible = "divisible"
prettyDescentStatus Isolated = "isolated"
prettyDescentStatus Cycle = "cycle"
prettyDescentStatus (Descent to) = "descent to " ++ prettyWord to

prettyDescentInfo :: [Label] -> DescentStatus -> String
prettyDescentInfo _ Divisible = "divisible"
prettyDescentInfo _ Isolated = "isolated"
prettyDescentInfo w Cycle = "cyc to " ++ prettyWord (turnForward w)
prettyDescentInfo _ (Descent to) = "desc to " ++ prettyWord to

type DescentTree = WordTree (Maybe DescentStatus)

emptyDescentTree :: DescentTree
emptyDescentTree = wordTreeFromFunction fct where
  fct word = if isDivisible word
                then Just Divisible
                else Nothing

baseDescentTree :: DescentTree
baseDescentTree = dt2 where
  dt1 = insertNode [One] Isolated (insertNode [Zero] Isolated emptyDescentTree)
  dt2 = addAscentOfWord [One] (addAscentOfWord [Zero] dt1)

lookupWord :: [Label] -> DescentTree -> DescentStatus
lookupWord word dt =
  fromMaybe (error $ "trying to lookup word (" ++ prettyWord word ++ ") that is not yet in descent tree.")
            (labelOfWord dt word)

insertNode :: [Label] -> DescentStatus -> DescentTree -> DescentTree
insertNode word status dt = {-trace ("insert " ++ show status ++ " at " ++ prettyWord word) $-} updateWord updater word dt where
  updater Nothing = Just status
  updater (Just _) = error "trying to insert word in descent tree that already has an entry"

descentSuccessorMaybe :: DescentTree -> [Label] -> Maybe [Label]
descentSuccessorMaybe dt word = case lookupWord word dt of
  Divisible -> Nothing
  Isolated  -> Nothing
  Cycle     -> Just $ turnForward word
  Descent w -> Just w

descentSuccessor :: DescentTree -> [Label] -> [Label]
descentSuccessor dt word = case lookupWord word dt of
  Divisible -> error "trying to descent from divisible"
  Isolated  -> error "trying to descent from isolated"
  Cycle     -> turnForward word
  Descent w -> w

descentPathTo :: DescentTree -> Label -> [Label] -> [Label]
descentPathTo dt target toWalkFrom =
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
          descentPathTo dt target (descentSuccessor dt toWalkFrom)

addCycleOfWord :: [Label] -> DescentTree -> DescentTree
addCycleOfWord word dt = let
    wordsOnCycle = turns word
    descentInformation wd = (labelOfWord dt wd) >>= maybeDescent
    descentPoints = filter (isJust .  descentInformation) wordsOnCycle
  in assert (not $ isDivisible word) $
     if null descentPoints
       then foldl (\dtree w -> insertNode w Isolated dtree) dt wordsOnCycle
       else {-trace (show descentPoints ++ "\n") $-} foldl (flip addPredecessorsOnCycle) dt descentPoints

ascentNode :: DescentTree -> [Label] -> [Label]
ascentNode dt word = newNode where
  firstLetter = case last word of Zero -> One
                                  One  -> Zero
  ascentPath = descentPathTo dt firstLetter word
  newNode = firstLetter : ascentPath

addAscentOfWord :: [Label] -> DescentTree -> DescentTree
addAscentOfWord word dt = dt' where
  newNode = ascentNode dt word
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
  
gathersMaybe :: DescentTree -> [Label] -> Maybe [Label]
gathersMaybe _ [Zero] = Just [Zero]
gathersMaybe _ [One]  = Just [One]
gathersMaybe dt word  = do descSuc <- descentSuccessorMaybe dt word
                           descGather <- gathersMaybe dt descSuc
                           return $ head word : descGather

gathers :: DescentTree -> [Label] -> [Label]
gathers _ [Zero] = [Zero]
gathers _ [One]  = [One]
gathers dt word  = head word : descGather where
  descGather = gathers dt descSuc
  descSuc = descentSuccessor dt word

immediatelyGatheredBy :: DescentTree -> [Label] -> [Label]
immediatelyGatheredBy dt word = let
    searchList = reverse $ inits word
    gathersMe gath = case gathersMaybe dt gath of
                       Nothing    -> False
                       Just gthrs -> gthrs `isPrefixOf` word
  in case find gathersMe searchList of
       Just  g -> g
       Nothing -> error $ "the word " ++ prettyWord word ++ " is not gathered"

immediatelyGathers :: DescentTree -> [Label] -> [[Label]]
immediatelyGathers dt word = generate [word] where
  generate [] = []
  generate (next:rest) = let
      exts = map (\l -> next ++ [l]) labelsList
      keep can = isJust (labelOfWord dt can) &&
                   word == immediatelyGatheredBy dt can
      keeps = filter keep exts
    in keeps ++ (generate (rest ++ keeps))
