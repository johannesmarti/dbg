module LabeledGraph (
  Label(..),
  labels,
  Arc,
  LabeledGraphI,
  MapFunction,
  domain, successors, predecessors, hasArc, arcs, arcsOfLabel, prettyNode,
  interfaceFromAll,
  interfaceFromSuccPredPretty,
  noPredecessor,
  hasDoubleRefl,
  showLG,
  prettyLabeledGraph,
  prettyPredLabeledGraph,
) where

import Control.Exception.Base
import Data.List (intercalate)
import Data.Set as Set

import Label
import Pretty


type MapFunction x = Label -> x -> Set x

data LabeledGraphI g x = LabeledGraphI {
  domain       :: g -> Set x,
  successors   :: g -> MapFunction x,
  predecessors :: g -> MapFunction x,
  hasArc       :: g -> Label -> (x,x) -> Bool,
  arcsOfLabel  :: g -> Label -> [(x,x)],
  prettyNode   :: g -> x -> String
}

interfaceFromAll :: (g -> Set x) -> (g -> MapFunction x) -> (g -> MapFunction x)
                    -> (g -> Label -> (x,x) -> Bool) -> (g -> Label -> [(x,x)])                     -> (g -> x -> String) -> LabeledGraphI g x
interfaceFromAll dom succ pred hasAr ar pretty =
  LabeledGraphI dom succ pred hasAr ar pretty

hasArcFromSucc :: Ord x => (g -> MapFunction x) -> g -> Label -> (x,x) -> Bool
hasArcFromSucc succ g l (v,w) = w `Set.member` (succ g l v)

aOLFromSucc :: Ord x => (g -> Set x) -> (g -> MapFunction x) -> g -> Label -> [(x,x)]
aOLFromSucc dom succs graph label =
  [(x,y) | x <- d, y <- Set.toList $ succs graph label x] where
    d = Set.toList $ dom graph

interfaceFromSuccPredPretty :: Ord x => (g -> Set x) -> (g -> MapFunction x)
  -> (g -> MapFunction x) -> (g -> x -> String) -> LabeledGraphI g x
interfaceFromSuccPredPretty dom succ pred pretty =
  LabeledGraphI dom succ pred (hasArcFromSucc succ)
                              (aOLFromSucc dom succ) pretty


arcs :: LabeledGraphI g x -> g -> [Arc x]
arcs gi g = concatMap fromLabel labels where
  fromLabel l = Prelude.map (insert l) (arcsOfLabel gi g l) 
  insert l (x,y) = (x,l,y)

noPredecessor :: Ord x => LabeledGraphI g x -> g -> x -> Bool
noPredecessor gi g node = any (\l -> Prelude.null $ predecessors gi g l node) labels

hasDoubleRefl :: Ord x => LabeledGraphI g x -> g -> Bool
hasDoubleRefl gi g = let
    hasBothLoops n = all (\l -> n `Set.member` predecessors gi g l n) labels
  in any hasBothLoops (domain gi g)


prettyLabeledGraph :: Ord x => LabeledGraphI g x -> g -> [String]
prettyLabeledGraph gi g = basePrinter gi printNode (stdPrintSet printNode) g where
  printNode = prettyNode gi g

basePrinter :: Ord x => LabeledGraphI g x -> (x -> String) -> ([x] -> String) -> g -> [String]
basePrinter gi printNode printSuccessors g = let
    succsForLabel v l lrep = " <" ++ lrep ++ " " ++
                             (printSuccessors (Set.toList (successors gi g l v)))
    lineForNode v = (printNode v) ++ succsForLabel v Zero "0"
                                  ++ succsForLabel v One "1"
  in fmap lineForNode (Set.toList . (domain gi) $ g)

prettyPredLabeledGraph :: Ord x => LabeledGraphI g x -> g -> [String]
prettyPredLabeledGraph gi g = basePredPrinter gi printNode (stdPrintSet printNode) g where
  printNode = prettyNode gi g

basePredPrinter :: Ord x => LabeledGraphI g x -> (x -> String) -> ([x] -> String) -> g -> [String]
basePredPrinter gi printNode printSuccessors g = let
    predsForLabel v l lrep = (printSuccessors (Set.toList (predecessors gi g l v))) ++ " <" ++ lrep ++ " "
    lineForNode v = predsForLabel v Zero "0"
                    ++ predsForLabel v One "1" ++ (printNode v)
  in fmap lineForNode (Set.toList . (domain gi) $ g)

showLG :: Ord x => LabeledGraphI g x -> g -> String
showLG gi = unlines . (prettyLabeledGraph gi)
