module Graph (
  GraphI,
  domain, successors, predecessors, hasArc,
  interfaceFromSuccPredPretty,
  interfaceFromHasArcPretty,
  interfaceFromArcsPretty,
  arcs,
  succPredInDom,
  succPredMatch,
  prettyGraph,
  prettyPredGraph,
) where

import Control.Exception.Base
import Data.List (intercalate)
import Data.Set as Set

data GraphI g x = GraphI {
  domain       :: g -> Set x,
  successors   :: g -> x -> Set x,
  predecessors :: g -> x -> Set x,
  hasArc       :: g -> (x,x) -> Bool,
  arcs         :: g -> [(x,x)],
  prettyNode   :: g -> x -> String
}

hasArcFromSucc :: Ord x => (g -> x -> Set x) -> (g -> (x,x) -> Bool)
hasArcFromSucc succ g (v,u) = u `Set.member` succ g v

arcsFromSucc :: Ord x => (g -> Set x) -> (g -> x -> Set x) -> g -> [(x,x)]
arcsFromSucc dom succ g = [(x,y) | x <- Set.toList $ dom g,
                                   y <- Set.toList $ succ g x]

succFromHasArc :: Ord x => (g -> Set x) -> (g -> (x,x) -> Bool) -> (g -> x -> Set x)
succFromHasArc dom hasAr g v = Set.filter (\u -> hasAr g (v,u)) (dom g)

predFromHasArc :: Ord x => (g -> Set x) -> (g -> (x,x) -> Bool) -> (g -> x -> Set x)
predFromHasArc dom hasAr g v = Set.filter (\u -> hasAr g (u,v)) (dom g)

arcsFromHasArc :: Ord x => (g -> Set x) -> (g -> (x,x) -> Bool) -> (g -> [(x,x)])
arcsFromHasArc dom hasAr g = [(x,y) | x <- d, y <- d, hasAr g (x,y)] where
  d = Set.toList $ dom g


interfaceFromSuccPredPretty :: Ord x => (g -> Set x)
  -> (g -> x -> Set x) -> (g -> x -> Set x) -> (g -> x -> String) -> GraphI g x
interfaceFromSuccPredPretty dom succ pred pretty =
  GraphI dom succ pred (hasArcFromSucc succ) (arcsFromSucc dom succ) pretty

interfaceFromHasArcPretty :: Ord x => (g -> Set x)
  -> (g -> (x,x) -> Bool) -> (g -> x -> String) -> GraphI g x
interfaceFromHasArcPretty dom hasAr pretty =
  GraphI dom (succFromHasArc dom hasAr) (predFromHasArc dom hasAr) hasAr (arcsFromHasArc dom hasAr) pretty

interfaceFromArcsPretty :: Ord x => (g -> [(x,x)]) -> (g -> x -> String) -> GraphI g x
interfaceFromArcsPretty ar pretty = GraphI dom succ pred hasAr ar pretty where
  dom g = Set.fromList (fmap fst arcs) `Set.union`
          Set.fromList (fmap snd arcs) where
              arcs = ar g
  {- Here we assume that the domain is the active domain -}
  succ g v = Set.fromList $ Prelude.map snd $ Prelude.filter (\(x,y) -> x == v) (ar g)
  pred g v = Set.fromList $ Prelude.map fst $ Prelude.filter (\(x,y) -> y == v) (ar g)
  hasAr g arc = arc `elem` (ar g)

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
