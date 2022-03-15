module LiftedGraphReport (
  liftedGraphReport,
  easyLiftedGraphReport,
) where

import qualified Data.Set as Set
import Data.List (maximumBy,intercalate)

import Graph
import Label
import LiftedGraph
import PairGraph
import BitGraph
import RelationTree
import WordTree

liftedGraphReport :: LiftedGraph x -> [String]
liftedGraphReport lg = let
    (lbg,s) = LiftedGraph.toLBitGraph lg
    wt = relationTree (lbg,s)
    wordRelList = allWordsWithout wt (hasUniv s)

    --onCycles = firstArcsOnCycles (lbg, s) wt
    onCycles w = Set.fromList $ arcsOnCycles (lbg, s) wt w

    cans = liftableCandidates (graph lg)

    printRel r = prettyGraph (bitGraphI s) r
    printWordWithRel (w,r) = let
        arcsOnCycles = onCycles w
        intersectingCans = map extractPair $ filter intersectsCycles cans
        intersects a b = not (a `Set.disjoint` b)
        intersectsCycles can = (Set.fromList $ labeledArcsOfCandidate can)
                                 `intersects` arcsOnCycles
      in [show w ++ ":"] ++ printRel (label r) ++
              [show (Set.toList arcsOnCycles), show (intersectingCans)]
  in intercalate [""] (map printWordWithRel wordRelList)

easyLiftedGraphReport :: LiftedGraph x -> IO ()
easyLiftedGraphReport = putStr . unlines . liftedGraphReport
