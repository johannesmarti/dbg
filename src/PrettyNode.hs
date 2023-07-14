-- These are needed to make String an instance of Pretty
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module PrettyNode (
  PrettyNode,
  pretty,
  stdPrintSet,
) where

import Data.List (intercalate)
import Data.Set as Set

{- The Pretty class determines how datatypes are pretty printed as parts of graphs. -}

class PrettyNode x where
  pretty :: x -> String

instance PrettyNode Char where
  pretty c = [c]

instance PrettyNode Int where
  pretty i = show i

instance PrettyNode Word where
  pretty i = show i

instance PrettyNode String where
  pretty i = i

stdPrintSet :: (a -> String) -> Set a -> String
stdPrintSet printSuccessor successors =
  --"{" ++ (intercalate "," (fmap printSuccessor successors)) ++ "}"
  "{" ++ (intercalate ", " (fmap printSuccessor (Set.toList successors))) ++ "}"
