module Path (
  Path(..),
  start,
  labelList,
  nodeList,
  cycleNodeList,
  prettyPath,
  prettyReversePath,
) where

import Label

data Path a = There a | Step a Label (Path a)
  deriving (Eq,Show)

instance Functor Path where
  fmap f (There a) = There (f a)
  fmap f (Step a l cont) = Step (f a) l (fmap f cont)

start :: Path a -> a
start (There x) = x
start (Step x _ _) = x

labelList :: Path a -> [Label]
labelList (There _) = []
labelList (Step _ l cont) = l : labelList cont

nodeList :: Path a -> [a]
nodeList (There x) = [x]
nodeList (Step x _ cont) = x : nodeList cont

cycleNodeList :: Path a -> [a]
cycleNodeList (There x) = []
cycleNodeList (Step x _ cont) = x : cycleNodeList cont

{- This function could probabely done more efficiently using ShowS -}
prettyPath :: (a -> String) -> Path a -> String
prettyPath nodePrinter (There a) = nodePrinter a
prettyPath nodePrinter (Step a l cont) = 
  nodePrinter a ++ "  " ++ labelToSymbol l ++ ">  " ++ (prettyPath nodePrinter cont)

prettyReversePath :: (a -> String) -> Path a -> String
prettyReversePath nodePrinter (There a) = nodePrinter a
prettyReversePath nodePrinter (Step a l cont) = 
  nodePrinter a ++ "  <" ++ labelToSymbol l ++ "  " ++ (prettyReversePath nodePrinter cont)
