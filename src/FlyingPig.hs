module FlyingPig (

) where

import Label
import Path
import Word

type Word = [Label]

data ExpansionData = ExpansionData {
  node    :: [Label], -- node determines the other data members
  epsilon :: ExpansionData,
  down    :: Path ExpansionData
}

fullPathDown :: ExpansionData -> Path ExpansionData
fullPathDown expd = Step expd (first . node $ expd) (down expd)

type Expander = Label -> ExpansionData -> ExpansionData

predecessor :: Expander -> Expander
predecessor expander label expd = let
    movedEpsilon = (expander label) (epsilon expd)
    pathToEpsilons = undefined
    extendedPath = fullPathDown expd
  in if last (node expd) == label
       then undefined -- need one more case distinction
     else {- expand -} let new = ExpansionData pathToEpsilons
                                               movedEpsilon
                                               extendedPath
                         in new
