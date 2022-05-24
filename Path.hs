module Path (
  Path(..),
  prettyPath,
) where

import Label

data Path a = There a | Step a Label (Path a)
  deriving Show

instance Functor Path where
  fmap f (There a) = There (f a)
  fmap f (Step a l cont) = Step (f a) l (fmap f cont)

{- This function could probabely done more efficiently using ShowS -}
prettyPath :: (a -> String) -> Path a -> String
prettyPath nodePrinter (There a) = nodePrinter a
prettyPath nodePrinter (Step a l cont) = 
  nodePrinter a ++ "  " ++ labelToSymbol l ++ ">  " ++ (prettyPath nodePrinter cont)