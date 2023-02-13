module FlyingPig (

) where

import Control.Exception.Base

import Label
import Path
import Word

type Word = [Label]

data ExpansionData = ExpansionData {
  node    :: [Label], -- node determines the other data members
  epsilon :: ExpansionData,
  down    :: Path ExpansionData
}

instance Eq ExpansionData where
  ea == eb = if node ea == node eb
               then assert (epsilon ea == epsilon eb && down ea == down eb)
                           True
               else False

inEpsilonStack :: ExpansionData -> ExpansionData -> Bool
inEpsilonStack ed stack =
  if ed == stack then True else inEpsilonStack ed (epsilon stack)

pathToEpsilon :: ExpansionData -> Path ExpansionData -> [Label]
pathToEpsilon epsilonStack (Step curr label cont) =
  if curr `inEpsilonStack` epsilonStack
    then []
    else label : (pathToEpsilon epsilonStack cont)
pathToEpsilon _ (There _) = error "something is really wrong"

fullPathDown :: ExpansionData -> Path ExpansionData
fullPathDown expd = Step expd (first . node $ expd) (down expd)

type Expander = Label -> ExpansionData -> ExpansionData

predecessor :: Expander -> Expander
predecessor expander label expd = let
    movedEpsilon = (expander label) (epsilon expd)
    pathToEpsilons = label : (pathToEpsilon movedEpsilon extendedPath)
    extendedPath = fullPathDown expd
  in if last (node expd) == label
       then let turned = turnBackward (node expd)
            in if turned == node movedEpsilon
                 then movedEpsilon
                 else ExpansionData turned movedEpsilon extendedPath
     else assert (pathToEpsilons /= node movedEpsilon) $
                 ExpansionData pathToEpsilons movedEpsilon extendedPath
