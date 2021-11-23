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
  hasDoubleRefl,
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

arcs :: Ord x => GraphI g x -> g -> [Arc x]
arcs gi graph = concatMap arcsForLabel labels where
  dom = Set.toList $ domain gi graph
  arcsForLabel l = [(x,l,y) | x <- dom, y <- Set.toList $ successors gi graph l x]

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

hasDoubleRefl :: Ord x => GraphI g x -> g -> Bool
hasDoubleRefl gi g = let
    hasBothLoops n = all (\l -> n `Set.member` predecessors gi g l n) labels
  in any hasBothLoops (domain gi g)

prettyPredGraph :: Ord x => GraphI g x -> (x -> String) -> g -> [String]
prettyPredGraph gi printNode g = basePredPrinter gi printNode (stdPrintSuccessors printNode) g

basePredPrinter :: Ord x => GraphI g x -> (x -> String) -> ([x] -> String) -> g -> [String]
basePredPrinter gi printNode printSuccessors g = let
    predsForLabel v l lrep = (printSuccessors (Set.toList (predecessors gi g l v))) ++ " <" ++ lrep ++ " "
    lineForNode v = predsForLabel v Zero "0"
                    ++ predsForLabel v One "1" ++ (printNode v)
  in fmap lineForNode (Set.toList . (domain gi) $ g)

