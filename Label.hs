module Label (
  Label(..),
  Arc(..),
  labels,
  labelToSymbol,
) where

import Data.Set

data Label = Zero | One
  deriving (Eq,Ord,Show)

type Arc x = (x,Label,x)

labels :: Set Label
labels = fromList [Zero, One]

labelToSymbol :: Label -> String
labelToSymbol Zero = "Z"
labelToSymbol One  = "O"
