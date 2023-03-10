module FlyingPig (
  PigNode,
  turningWord,parent,
  epsilon,zero,one,
  predecessor,
  address,
  lookupAddress,
) where

import Control.Exception.Base
import Data.Function (fix)

import Label
import Path
import Word

data PigNode = PigNode {
  turningWord :: [Label], -- turningWord determines the other data members
  parent      :: PigNode,
  pathDown    :: Path PigNode
}

deepEqual :: PigNode -> PigNode -> Bool
deepEqual (PigNode twa paa pda) (PigNode twb pab pdb) =
  if null twa then null twb
  else if null twb then False
  else twa == twb &&
       deepEqual paa pab &&
       pda == pdb -- actually we should call deep equal recursively here. but that is too much of a hassle

instance Eq PigNode where
  ea == eb = if turningWord ea == turningWord eb
               then assert (deepEqual ea eb)
                           True
               else False

instance Show PigNode where
  show (PigNode tw par _) = "[ " ++ prettyWord tw ++ " | " ++
                                    prettyWord (turningWord par) ++ " ]"

epsilon :: PigNode
epsilon = PigNode [] epsilon (There epsilon)

zero :: PigNode
zero = PigNode [Zero] epsilon (There epsilon)

one :: PigNode
one = PigNode [One] epsilon (There epsilon)

isAncestor :: PigNode -> PigNode -> Bool
isAncestor curr stack =
  if curr == stack then True
  else if stack == epsilon then False
  else isAncestor curr (parent stack)

pathToAncestor :: PigNode -> Path PigNode -> [Label]
pathToAncestor parentStack (Step curr label cont) =
  if curr `isAncestor` parentStack
    then []
    else label : (pathToAncestor parentStack cont)
pathToAncestor ps (There here) = assert (here == epsilon) $
                                 assert (here `isAncestor` ps) $ []
                                        

fullPathDown :: PigNode -> Path PigNode
fullPathDown expd = Step expd (first . turningWord $ expd) (pathDown expd)

type Expander = Label -> PigNode -> PigNode

looper :: Expander -> Expander
looper expander label expd = let
    movedEpsilon = (expander label) (parent expd)
    pathToAncestors = label : (pathToAncestor movedEpsilon extendedPath)
    extendedPath = fullPathDown expd
  in if expd == epsilon then case label of Zero -> zero
                                           One  -> one
     else if last (turningWord expd) == label
       then let turned = turnBackward (turningWord expd)
            in if turned == turningWord movedEpsilon
                 then movedEpsilon
                 else PigNode turned movedEpsilon extendedPath
     else assert (pathToAncestors /= turningWord movedEpsilon) $
                 PigNode pathToAncestors movedEpsilon extendedPath

predecessor :: Expander
predecessor = fix looper

address :: PigNode -> [Label]
address (PigNode [] x y) = assert (PigNode [] x y == epsilon) []
address (PigNode w _ p) = addressWorker p [(first w)]

addressWorker :: (Path PigNode) -> [Label] -> [Label]
addressWorker (There x) accum = assert (x == epsilon) accum
addressWorker (Step _ label cont) accum = addressWorker cont (label:accum)

lookupAddress :: [Label] -> PigNode
lookupAddress = foldl (flip predecessor) epsilon
