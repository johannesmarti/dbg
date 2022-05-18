module LabeledGraph (
  Label(..),
  labels,
  Arc,
  LabeledGraphI,
  MapFunction,
  domain, successors, predecessors, hasArc, arcs, arcsOfLabel, prettyNode,
  interfaceFromAll,
  interfaceFromSuccPredPretty,
  interfaceFromHasArcPretty,
  noPredecessor,
  hasT1,
  hasBothLoops,
  hasDoubleRefl,
  showLG,
  prettyLabeledGraph,
  prettyPredLabeledGraph,
  prettyBigLabeledGraph,
  prettierBigLabeledGraph,
) where

import Control.Exception.Base
import Data.List (intercalate)
import Data.Set as Set

import Label
import Pretty
import Tools


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


succFromHasArc :: Ord x => (g -> Set x) -> (g -> Label -> (x,x) -> Bool) -> (g -> Label -> x -> Set x)
succFromHasArc dom hasAr g l v = Set.filter (\u -> hasAr g l (v,u)) (dom g)

predFromHasArc :: Ord x => (g -> Set x) -> (g -> Label -> (x,x) -> Bool) -> (g -> Label -> x -> Set x)
predFromHasArc dom hasAr g l v = Set.filter (\u -> hasAr g l (u,v)) (dom g)

arcsFromHasArc :: Ord x => (g -> Set x) -> (g -> Label -> (x,x) -> Bool) -> (g -> Label -> [(x,x)])
arcsFromHasArc dom hasAr g l = [(x,y) | x <- d, y <- d, hasAr g l (x,y)] where
  d = Set.toList $ dom g


interfaceFromSuccPredPretty :: Ord x => (g -> Set x) -> (g -> MapFunction x)
  -> (g -> MapFunction x) -> (g -> x -> String) -> LabeledGraphI g x
interfaceFromSuccPredPretty dom succ pred pretty =
  LabeledGraphI dom succ pred (hasArcFromSucc succ)
                              (aOLFromSucc dom succ) pretty

interfaceFromHasArcPretty :: Ord x => (g -> Set x)
  -> (g -> Label -> (x,x) -> Bool) -> (g -> x -> String) -> LabeledGraphI g x
interfaceFromHasArcPretty dom hasAr pretty =
  LabeledGraphI dom (succFromHasArc dom hasAr) (predFromHasArc dom hasAr) hasAr (arcsFromHasArc dom hasAr) pretty

arcs :: LabeledGraphI g x -> g -> [Arc x]
arcs gi g = concatMap fromLabel labels where
  fromLabel l = Prelude.map (insert l) (arcsOfLabel gi g l) 
  insert l (x,y) = (x,l,y)

noPredecessor :: Ord x => LabeledGraphI g x -> g -> x -> Bool
noPredecessor gi g node = any (\l -> Prelude.null $ predecessors gi g l node) labels

hasBothPredecessorDom :: Ord x => LabeledGraphI g x -> g -> Set x
hasBothPredecessorDom gi g = Set.filter (\n -> not (noPredecessor gi g n)) (domain gi g)

hasBothLoops :: Ord x => LabeledGraphI g x -> g -> x -> Bool
hasBothLoops gi g n = all (\l -> hasArc gi g l (n, n)) labels

hasDoubleRefl :: Ord x => LabeledGraphI g x -> g -> Bool
hasDoubleRefl gi g = any (hasBothLoops gi g) (domain gi g)

hasT1 :: Ord x => LabeledGraphI g x -> g -> Bool
hasT1 gi g = let
    dom = domain gi g
    isRefl l v = hasArc gi g l (v,v)
    zeroRefl = Set.filter (isRefl Zero) dom
    oneRefl  = Set.filter (isRefl One)  dom
    pairs = Set.cartesianProduct zeroRefl oneRefl
    isConnected (z,o) = hasArc gi g Zero (z,o) && hasArc gi g One (o,z)
  in any isConnected pairs

prettyLabeledGraph :: Ord x => LabeledGraphI g x -> g -> [String]
prettyLabeledGraph gi g = basePrinter gi printNode (stdPrintSet printNode) g where
  printNode = prettyNode gi g

basePrinter :: Ord x => LabeledGraphI g x -> (x -> String) -> (Set x -> String) -> g -> [String]
basePrinter gi printNode printSuccessors g = let
    succsForLabel v l lrep = " <" ++ lrep ++ " " ++
                             (printSuccessors (successors gi g l v))
    lineForNode v = (printNode v) ++ succsForLabel v Zero "0"
                                  ++ succsForLabel v One "1"
  in Prelude.map lineForNode (Set.toList (domain gi g))

prettyPredLabeledGraph :: Ord x => LabeledGraphI g x -> g -> [String]
prettyPredLabeledGraph gi g = basePredPrinter gi printNode (stdPrintSet printNode) g where
  printNode = prettyNode gi g

basePredPrinter :: Ord x => LabeledGraphI g x -> (x -> String) -> (Set x -> String) -> g -> [String]
basePredPrinter gi printNode printSuccessors g = let
    predsForLabel v l lrep = (printSuccessors (predecessors gi g l v)) ++ " <" ++ lrep ++ " "
    lineForNode v = predsForLabel v Zero "0"
                    ++ predsForLabel v One "1" ++ (printNode v)
  in Prelude.map lineForNode (Set.toList (domain gi g))

prettyBigLabeledGraph :: Ord x => LabeledGraphI g x -> g -> [String]
prettyBigLabeledGraph gi g = baseBigPrinter gi printNode (stdPrintSet printNode) g where
  printNode = prettyNode gi g

prettierBigLabeledGraph :: Ord x => LabeledGraphI g x -> g -> (x -> String) -> (x -> String) -> [String]
prettierBigLabeledGraph gi g printNode printNodeInSet =
  baseBigPrinter gi printNode (stdPrintSet printNodeInSet) g

baseBigPrinter :: Ord x => LabeledGraphI g x -> (x -> String) -> (Set x -> String) -> g -> [String]
baseBigPrinter gi printNode printSuccessors g = let
    succsForLabel v l lrep = " <" ++ lrep ++ " " ++
                   (printSuccessors (successors gi g l v))
    lineForZero v = printNode v ++ succsForLabel v Zero "0"
    lineForOne  v = printNode v ++ succsForLabel v One "1"
    d = Set.toList $ domain gi g
  in Prelude.map lineForZero d ++ [""] ++ Prelude.map lineForOne d

showLG :: Ord x => LabeledGraphI g x -> g -> String
showLG gi = unlines . (prettyBigLabeledGraph gi)
