{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Graph (
  Label(..),
  labels,
  Arc,
  Graph,
  MapFunction,
  domain, successors, predecessors,
  fromFunctionsWithNodePrinter,
  fromFunctions,
  arcs,
) where

import Control.Exception.Base
import Data.List (intercalate)
import Data.Set as Set

data Label = Zero | One
  deriving (Eq,Ord,Show)

type Arc x = (x,Label,x)

labels :: Set Label
labels = Set.fromList [Zero, One]

type MapFunction x = Label -> x -> Set x

class Graph g x where
  domain       :: g -> Set x
  successors   :: g -> MapFunction x
  predecessors :: g -> MapFunction x

foldConverse :: Ord y => Set x -> (x -> Set y) -> (y -> Set x)
foldConverse dom fct v = Set.filter (\u -> v `Set.member` fct u) dom

sameOnDom :: (Ord x, Eq y) => Set x -> (x -> y) -> (x -> y) -> Bool
sameOnDom dom f g = all (\a -> f a == g a) dom

graphCompatibility :: (Ord x, Graph g x) => g -> Bool
graphCompatibility graph = 
 (all ((`isSubsetOf` dom) . (uncurry succ)) product) &&
 (all ((`isSubsetOf` dom) . (uncurry pred)) product) &&
 (sameOnDom product (uncurry pred) (uncurry pred')) where
    succ = successors graph
    pred = predecessors graph
    dom = domain graph
    pred' l = foldConverse dom (succ l)
    product = cartesianProduct labels dom

arcs :: (Ord x, Graph g x) => g -> [Arc x]
arcs graph = concatMap arcsForLabel labels where
  dom = Set.toList $ domain graph
  arcsForLabel l = [(x,l,y) | x <- dom, y <- Set.toList $ successors graph l x]

{-
instance (Ord x, Show x) => Show (Graph x) where
  show = unlines . prettyGraph
-}

prettyGraph :: (Ord x, Graph g x) => (x -> String) -> g -> [String]
prettyGraph printNode g = basePrinter (printNode g) (stdPrintSuccessors (printNode g)) g

stdPrintSuccessors :: (a -> String) -> [a] -> String
stdPrintSuccessors printSuccessor successors =
  "{" ++ (intercalate "," (fmap printSuccessor successors)) ++ "}"

basePrinter :: (Ord x, Graph g x) => (x -> String) -> ([x] -> String) -> g -> [String]
basePrinter printNode printSuccessors g = let
    succsForLabel v l lrep = " <" ++ lrep ++ " " ++
                             (printSuccessors (Set.toList (successors g l v)))
    lineForNode v = (printNode v) ++ succsForLabel v Zero "0"
                                  ++ succsForLabel v One "1"
  in fmap lineForNode (Set.toList . domain $ g)
