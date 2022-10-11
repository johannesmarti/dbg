module Label (
  Label(..),
  Arc(..),
  labelsList,
  labels,
  labelToSymbol,
) where

import Data.Set

data Label = Zero | One
  deriving (Eq,Ord,Show)

type Arc x = (x,Label,x)

labelsList :: [Label]
labelsList = [Zero, One]

labels :: Set Label
labels = fromList labelsList

{-
labelToSymbol :: Label -> String
labelToSymbol Zero = "0"
labelToSymbol One  = "1"
-}

labelToSymbol :: Label -> String
labelToSymbol Zero = "z"
labelToSymbol One  = "o"
