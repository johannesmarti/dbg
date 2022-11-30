module Label (
  Label(..),
  Arc(..),
  labelsList,
  labels,
  labelToSymbol,
  prettyWord,
) where

import qualified Data.Set as Set

data Label = Zero | One
  deriving (Eq,Ord,Show)

type Arc x = (x,Label,x)

labelsList :: [Label]
labelsList = [Zero, One]

labels :: Set.Set Label
labels = Set.fromList labelsList

{-
labelToSymbol :: Label -> String
labelToSymbol Zero = "0"
labelToSymbol One  = "1"
-}

labelToSymbol :: Label -> String
labelToSymbol Zero = "z"
labelToSymbol One  = "o"

prettyWord :: [Label] -> String
prettyWord = map sym where
  sym Zero = '0'
  sym  One = '1'
