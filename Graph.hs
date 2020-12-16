module Graph (
  Label(..),
  labels,
  Graph,
  domain, successors, predecessors,
  fromFunctions,
) where

import Control.Exception.Base
import Data.Set as Set

data Label = Zero | One
  deriving (Eq,Ord,Show)

labels :: Set Label
labels = Set.fromList [Zero, One]

type MapFunction x = Label -> x -> Set x

data Graph x = Graph {
  domain       :: Set x,
  successors   :: MapFunction x,
  predecessors :: MapFunction x
}

fromFunctions :: Ord x => Set x -> MapFunction x -> MapFunction x -> Graph x
fromFunctions dom succ pred =
  assert (all ((`isSubsetOf` dom) . (uncurry succ)) product) $
  assert (all ((`isSubsetOf` dom) . (uncurry pred)) product) $
  assert (sameOnDom product (uncurry pred) (uncurry pred')) $
    (Graph dom succ pred) where
      pred' l = foldConverse dom (succ l)
      product = cartesianProduct labels dom

foldConverse :: Ord y => Set x -> (x -> Set y) -> (y -> Set x)
foldConverse dom fct v = Set.filter (\u -> v `Set.member` fct u) dom

sameOnDom :: (Ord x, Eq y) => Set x -> (x -> y) -> (x -> y) -> Bool
sameOnDom dom f g = all (\a -> f a == g a) dom
