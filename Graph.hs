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

data Graph x = Graph {
  domain       :: Set x,
  successors   :: MapFunction x,
  predecessors :: MapFunction x,
  printNode    :: x -> String
}

fromFunctions :: (Ord x, Show x) => Set x -> MapFunction x -> MapFunction x -> Graph x
fromFunctions = fromFunctionsWithNodePrinter show

foldConverse :: Ord y => Set x -> (x -> Set y) -> (y -> Set x)
foldConverse dom fct v = Set.filter (\u -> v `Set.member` fct u) dom

sameOnDom :: (Ord x, Eq y) => Set x -> (x -> y) -> (x -> y) -> Bool
sameOnDom dom f g = all (\a -> f a == g a) dom

fromFunctionsWithNodePrinter :: Ord x => (x -> String) -> Set x -> MapFunction x -> MapFunction x -> Graph x
fromFunctionsWithNodePrinter nodePrinter dom succ pred =
  assert (all ((`isSubsetOf` dom) . (uncurry succ)) product) $
  assert (all ((`isSubsetOf` dom) . (uncurry pred)) product) $
  assert (sameOnDom product (uncurry pred) (uncurry pred')) $
    Graph dom succ pred nodePrinter where
      pred' l = foldConverse dom (succ l)
      product = cartesianProduct labels dom

arcs :: Ord x => Graph x -> [Arc x]
arcs graph = concatMap arcsForLabel labels where
  dom = Set.toList $ domain graph
  arcsForLabel l = [(x,l,y) | x <- dom, y <- Set.toList $ successors graph l x]

instance (Ord x, Show x) => Show (Graph x) where
  show = unlines . prettyGraph

prettyGraph :: (Ord x, Show x) => Graph x -> [String]
prettyGraph g = basePrinter (printNode g) (stdPrintSuccessors (printNode g)) g

stdPrintSuccessors :: (a -> String) -> [a] -> String
stdPrintSuccessors printSuccessor successors =
  "{" ++ (intercalate "," (fmap printSuccessor successors)) ++ "}"

basePrinter :: Ord x => (x -> String) -> ([x] -> String) -> Graph x -> [String]
basePrinter printNode printSuccessors g = let
    succsForLabel v l lrep = " <" ++ lrep ++ " " ++
                             (printSuccessors (Set.toList (successors g l v)))
    lineForNode v = (printNode v) ++ succsForLabel v Zero "0"
                                  ++ succsForLabel v One "1"
  in fmap lineForNode (Set.toList . domain $ g)
