module Graph (
  Label(..),
  labels,
  Arc,
  GraphI(GraphI),
  MapFunction,
  wellDefined,
  domain, successors, predecessors,
  arcs,
  arcsOfLabel,
  noPredecessor,
  prettyGraph,
  prettyPredGraph,
  stdPrintSuccessors,
) where

import Control.Exception.Base
import Data.List (intercalate)
import Data.Set as Set

import Label

type MapFunction x = Label -> x -> Set x

data GraphI g x = GraphI {
  domain       :: g -> Set x,
  successors   :: g -> MapFunction x,
  predecessors :: g -> MapFunction x
}

foldConverse :: Ord y => Set x -> (x -> Set y) -> (y -> Set x)
foldConverse dom fct v = Set.filter (\u -> v `Set.member` fct u) dom

sameOnDom :: (Ord x, Eq y) => Set x -> (x -> y) -> (x -> y) -> Bool
sameOnDom dom f g = all (\a -> f a == g a) dom

wellDefined :: Ord x => GraphI g x -> g -> Bool
wellDefined gi graph = 
 (all ((`isSubsetOf` dom) . (uncurry succ)) product) &&
 (all ((`isSubsetOf` dom) . (uncurry pred)) product) &&
 (sameOnDom product (uncurry pred) (uncurry pred')) where
    succ = successors gi graph
    pred = predecessors gi graph
    dom = domain gi graph
    pred' l = foldConverse dom (succ l)
    product = cartesianProduct labels dom

arcs :: Ord x => GraphI g x -> g -> [Arc x]
arcs gi graph = concatMap arcsForLabel labels where
  dom = Set.toList $ domain gi graph
  arcsForLabel l = [(x,l,y) | x <- dom, y <- Set.toList $ successors gi graph l x]

arcsOfLabel :: Ord x => GraphI g x -> g -> Label -> [(x,x)]
arcsOfLabel gi g l = [(x,y) | x <- dom, y <- succs x] where
  dom = Set.toList $ domain gi g
  succs v = Set.toList $ successors gi g l v

{-
instance (Ord x, Show x) => Show (Graph x) where
  show = unlines . prettyGraph
-}

noPredecessor :: Ord x => GraphI g x -> g -> x -> Bool
noPredecessor gi g node = any (\l -> Prelude.null $ predecessors gi g l node) labels

prettyGraph :: Ord x => GraphI g x -> (x -> String) -> g -> [String]
prettyGraph gi printNode g = basePrinter gi printNode (stdPrintSuccessors printNode) g

stdPrintSuccessors :: (a -> String) -> [a] -> String
stdPrintSuccessors printSuccessor successors =
  --"{" ++ (intercalate "," (fmap printSuccessor successors)) ++ "}"
  "{" ++ (intercalate ", " (fmap printSuccessor successors)) ++ "}"

basePrinter :: Ord x => GraphI g x -> (x -> String) -> ([x] -> String) -> g -> [String]
basePrinter gi printNode printSuccessors g = let
    succsForLabel v l lrep = " <" ++ lrep ++ " " ++
                             (printSuccessors (Set.toList (successors gi g l v)))
    lineForNode v = (printNode v) ++ succsForLabel v Zero "0"
                                  ++ succsForLabel v One "1"
  in fmap lineForNode (Set.toList . (domain gi) $ g)

prettyPredGraph :: Ord x => GraphI g x -> (x -> String) -> g -> [String]
prettyPredGraph gi printNode g = basePredPrinter gi printNode (stdPrintSuccessors printNode) g

basePredPrinter :: Ord x => GraphI g x -> (x -> String) -> ([x] -> String) -> g -> [String]
basePredPrinter gi printNode printSuccessors g = let
    predsForLabel v l lrep = (printSuccessors (Set.toList (predecessors gi g l v))) ++ " <" ++ lrep ++ " "
    lineForNode v = predsForLabel v Zero "0"
                    ++ predsForLabel v One "1" ++ (printNode v)
  in fmap lineForNode (Set.toList . (domain gi) $ g)

