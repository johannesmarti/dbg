module Data.WordMaps.Algebraic (
  WordMap,
  empty,
  combine,
  inDomain,
  forceLookup,
  Data.WordMaps.Algebraic.lookup,
  insert,
  delete,
) where

import Data.Maybe (fromMaybe,isJust)

import Data.Label

data WordMap x = Empty | Branch (Maybe x) (WordMap x) (WordMap x)

empty :: WordMap x
empty = Empty

combine :: x -> WordMap x -> WordMap x -> WordMap x
combine lbl left right = Branch (Just lbl) left right

inDomain :: [Label] -> WordMap x -> Bool
inDomain word wm = isJust $ Data.WordMaps.Algebraic.lookup word wm

forceLookup :: [Label] -> WordMap x -> x
forceLookup key m = fromMaybe err (Data.WordMaps.Algebraic.lookup key m) where
  err = error $ "node " ++ show key ++ " not in WordMap"

lookup :: [Label] -> WordMap x -> Maybe x
lookup _ Empty = Nothing
lookup [] (Branch lbl _ _) = lbl
lookup (Zero:rest) (Branch _ left  _) = Data.WordMaps.Algebraic.lookup rest left
lookup (One :rest) (Branch _ _ right) = Data.WordMaps.Algebraic.lookup rest right

insert :: [Label] -> x -> WordMap x -> WordMap x
insert []          value Empty = Branch (Just value) Empty Empty
insert (Zero:rest) value Empty = Branch Nothing (insert rest value Empty) Empty
insert (One :rest) value Empty = Branch Nothing Empty (insert rest value Empty)
insert [] value (Branch _ left right) = Branch (Just value) left right
insert (Zero:rest) value (Branch lbl left right) =
  Branch lbl (insert rest value left) right
insert (One :rest) value (Branch lbl left right) =
  Branch lbl left (insert rest value right)

delete :: [Label] -> WordMap x -> WordMap x
delete _ Empty = Empty
delete [] (Branch _ Empty Empty) = Empty
delete [] (Branch _ left  right) = Branch Nothing left right
delete (Zero:rest) (Branch Nothing left Empty) = let
    left' = delete rest left
  in if isEmpty left' then Empty else Branch Nothing left' Empty
delete (Zero:rest) (Branch lbl left right) =
  Branch lbl (delete rest left) right
delete (One:rest) (Branch Nothing Empty right) = let
    right' = delete rest right
  in if isEmpty right' then Empty else Branch Nothing Empty right'
delete (One:rest) (Branch lbl left right) =
  Branch lbl left (delete rest right)

isEmpty :: WordMap x -> Bool
isEmpty Empty = True
isEmpty (Branch _ _ _) = False
