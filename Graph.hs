module Graph (
  GraphI,
  MapFunction,
  domain, successors, predecessors, hasArc,
  arcs,
  prettyGraph,
  prettyPredGraph,
) where

import Control.Exception.Base
import Data.List (intercalate)
import Data.Set as Set

type MapFunction x = x -> Set x

data GraphI g x = GraphI {
  domain       :: g -> Set x,
  successors   :: g -> MapFunction x,
  predecessors :: g -> MapFunction x,
  hasArc       :: g -> (x,x) -> Bool,
  prettyNode   :: g -> x -> String
}

succPredInDom :: Ord x => GraphI g x -> g -> Bool
succPredInDom gi graph =
 all ((`isSubsetOf` dom) . succ) dom &&
 all ((`isSubsetOf` dom) . pred) dom where
    succ = successors gi graph
    pred = predecessors gi graph
    dom = domain gi graph

foldConverse :: Ord y => Set x -> (x -> Set y) -> (y -> Set x)
foldConverse dom fct v = Set.filter (\u -> v `Set.member` fct u) dom

sameOnDom :: (Ord x, Eq y) => Set x -> (x -> y) -> (x -> y) -> Bool
sameOnDom dom f g = all (\a -> f a == g a) dom

succPredMatch :: Ord x => GraphI g x -> g -> Bool
succPredMatch gi graph = 
 (sameOnDom dom (predecessors gi graph) pred') where
    dom = domain gi graph
    pred' = foldConverse dom (successors gi graph)

arcs :: Ord x => GraphI g x -> g -> [(x,x)]
arcs gi g = [(x,y) | x <- Set.toList $ (domain gi g),
                     y <- Set.toList $ successors gi g x]


prettyGraph :: Ord x => GraphI g x -> g -> [String]
prettyGraph gi g = basePrinter gi printNode (stdPrintSet printNode) g where
  printNode = prettyNode gi g

stdPrintSet :: (a -> String) -> [a] -> String
stdPrintSet printSuccessor successors =
  --"{" ++ (intercalate "," (fmap printSuccessor successors)) ++ "}"
  "{" ++ (intercalate ", " (fmap printSuccessor successors)) ++ "}"

basePrinter :: Ord x => GraphI g x -> (x -> String) -> ([x] -> String) -> g -> [String]
basePrinter gi printNode printSuccessors g = let
    lineForNode v = (printNode v) ++ " < " ++ (printSuccessors (Set.toList $ successors gi g v))
  in fmap lineForNode (Set.toList . (domain gi) $ g)

prettyPredGraph :: Ord x => GraphI g x -> g -> [String]
prettyPredGraph gi g = basePredPrinter gi printNode (stdPrintSet printNode) g where
  printNode = prettyNode gi g

basePredPrinter :: Ord x => GraphI g x -> (x -> String) -> ([x] -> String) -> g -> [String]
basePredPrinter gi printNode printSuccessors g = let
    lineForNode v = (printSuccessors (Set.toList $ successors gi g v)) ++
                    " < " ++ (printNode v)
  in fmap lineForNode (Set.toList . (domain gi) $ g)
