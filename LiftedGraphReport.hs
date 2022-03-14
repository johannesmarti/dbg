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
import WordTree

liftedGraphReport :: LiftedGraph x -> [String]
liftedGraphReport lg = let
    s = size lg
    wt = relationTree lg
    wordRelList = allWordsWithout wt (hasUniv s)
    printRel r = prettyGraph (bitGraphI s) r
    printWordWithRel (w,r) = [show w ++ ":"] ++ printRel (label r)
  in intercalate [""] (map printWordWithRel wordRelList)

easyLiftedGraphReport :: LiftedGraph x -> IO ()
easyLiftedGraphReport = putStr . unlines . liftedGraphReport
