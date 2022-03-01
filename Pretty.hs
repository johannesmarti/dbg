-- These are needed to make String an instance of Pretty
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Pretty (
  Pretty,
  pretty,
  stdPrintSet,
) where

import Data.List (intercalate)

{- The Pretty class determines how datatypes are pretty printed as parts of graphs. -}

class Pretty x where
  pretty :: x -> String

instance Pretty Char where
  pretty c = [c]

instance Pretty Int where
  pretty i = show i

instance Pretty Word where
  pretty i = show i

instance Pretty String where
  pretty i = i

stdPrintSet :: (a -> String) -> [a] -> String
stdPrintSet printSuccessor successors =
  --"{" ++ (intercalate "," (fmap printSuccessor successors)) ++ "}"
  "{" ++ (intercalate ", " (fmap printSuccessor successors)) ++ "}"
