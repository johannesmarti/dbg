module CoveringGraph (
  CoveringNode,
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
import WordTree

data CoveringNode = CoveringNode {
  turningWord :: [Label], -- turningWord determines the other data members
  address     :: [Label],
  parent      :: CoveringNode,
  pathDown    :: Path CoveringNode
}

deepEqual :: CoveringNode -> CoveringNode -> Bool
deepEqual (CoveringNode twa aa paa pda) (CoveringNode twb ab pab pdb) =
  if null twa then null twb
  else if null twb then False
  else twa == twb &&
       deepEqual paa pab &&
       pda == pdb -- actually we should call deep equal recursively here. but that is too much of a hassle

instance Eq CoveringNode where
  ea == eb = if turningWord ea == turningWord eb
               then assert (deepEqual ea eb)
                           True
               else False

instance Show CoveringNode where
  show (CoveringNode tw _ par _) = "[ " ++ prettyWord tw ++ " | " ++
                                    prettyWord (turningWord par) ++ " ]"

epsilon :: CoveringNode
epsilon = CoveringNode [] [] epsilon (There epsilon)

zero :: CoveringNode
zero = CoveringNode [Zero] [Zero] epsilon (There epsilon)

one :: CoveringNode
one = CoveringNode [One] [One] epsilon (There epsilon)

isAncestor :: CoveringNode -> CoveringNode -> Bool
isAncestor curr stack =
  if curr == stack then True
  else if stack == epsilon then False
  else isAncestor curr (parent stack)

pathToAncestor :: CoveringNode -> Path CoveringNode -> [Label]
pathToAncestor parentStack (Step curr label cont) =
  if curr `isAncestor` parentStack
    then []
    else label : (pathToAncestor parentStack cont)
pathToAncestor ps (There here) = assert (here == epsilon) $
                                 assert (here `isAncestor` ps) $ []
                                        

fullPathDown :: CoveringNode -> Path CoveringNode
fullPathDown expd = Step expd (first . turningWord $ expd) (pathDown expd)

data AscentStatus a = Ascent a | LoopBack a

extractNode :: AscentStatus a -> a
extractNode (Ascent x) = x
extractNode (LoopBack x) = x

type Expander = Label -> CoveringNode -> (AscentStatus CoveringNode)

looper :: Expander -> Expander
looper expander label expd = let
    movedEpsilon = extractNode $ (expander label) (parent expd)
    pathToAncestors = label : (pathToAncestor movedEpsilon extendedPath)
    extendedPath = fullPathDown expd
    newAddress = address expd ++ [label]
  in if expd == epsilon then case label of Zero -> (Ascent zero)
                                           One  -> (Ascent one)
     else if last (turningWord expd) == label
       then let turned = turnBackward (turningWord expd)
            in if turned == turningWord movedEpsilon
                 then (LoopBack movedEpsilon)
                 else Ascent (CoveringNode turned newAddress
                 			   movedEpsilon extendedPath)
     else assert (pathToAncestors /= turningWord movedEpsilon) $
                 Ascent (CoveringNode pathToAncestors newAddress
                 	              movedEpsilon extendedPath)

predecessor :: Label -> CoveringNode -> CoveringNode
predecessor l n = extractNode $ treeCover l n

fixCover :: Expander
fixCover = fix looper

treeCover :: Expander
treeCover l node = atAddress ((address node) ++ [l])

atAddress :: [Label] -> AscentStatus CoveringNode
atAddress = labelOfWord cacheTree

cacheTree :: WordTree (AscentStatus CoveringNode)
cacheTree =  wordTreeFromGenerator generator where
  expandOn l n = looper treeCover l (extractNode n)
  generator = WordTreeGenerator (Ascent epsilon)
                (expandOn Zero)
                (expandOn One)

lookupAddress :: [Label] -> CoveringNode
lookupAddress = extractNode . atAddress

