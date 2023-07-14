module Report (
  cayleyReport, easyCayleyReport,
  spiralReport, easySpiralReport,
) where

import qualified Data.Set as Set
import Data.List (maximumBy,intercalate,intersperse)

import Word
import Data.Label
import Bitable
import LWrappedGraph
import qualified WrappedGraph as WG
import qualified Data.Path as Path
import LabeledGraph
import CommonLGraphTypes
import CayleyGraph hiding (relationOfWord)
import BitGraph
import Coding
import Pretty
import Graph as Graph
import RelationCache
import Lifting
import DeterminismProperty
import PathTree
import Spiral

import Tools

cayleyReport :: Ord x => LabeledGraphI g x -> g -> [String]
cayleyReport gi g = let
    bf = (genericBitableI gi) g
    s = numBits bf
    inner = labeledBitGraph bf
    cg = rightCayleyGraph bf
    c = Bitable.coding bf
    dec = decode c
    printRel r = Graph.prettyGraph (relationI bf) r
    printRelWithCode r = (printNodeWithSuccs cg r ++ ":") : (printRel r)
    wfs = wellfoundedElements cg
    nwfs = nonWellfoundedElements cg
    finWords = finiteWords s cg
    (longestFinWord,relOfLongest) = maximumBy (\(a,_) (b,_) -> compare (length a) (length b)) finWords
  in ["About the Cayley graph of the pattern:"] ++
      LabeledGraph.prettyLabeledGraph gi g ++
     ["It has " ++ show (Set.size wfs) ++ " finite and " ++
                   show (Set.size nwfs) ++ " infinite elements.", "",
      "It " ++ (if isConstructionDeterministic gi g then "is" else "is not") ++ " construction deterministic.", "",
      "It " ++ (if pathCondition s cg then "satisfies" else "does not satisfy") ++ " the path condition.", "",
      --"It's finite words are:", show (map fst finWords),
      "Of which one with maximal length is " ++ show longestFinWord ++ ".", ""] ++
     ["The full CayleyGraph is:"] ++ prettyCayleyGraph cg ++
     ["", "The complete list of its finite elements is:"] ++
      concatMap printRelWithCode (Set.toList wfs) ++
     ["", "The complete list of its infinite elements is:"] ++
      concatMap printRelWithCode (Set.toList nwfs)
     
easyCayleyReport :: Ord x => LabeledGraphI g x -> g -> IO ()
easyCayleyReport gi g = putStr . unlines $ cayleyReport gi g

cyclesOfWord :: Ord x => RelationCache r x -> [Label] -> [[x]]
cyclesOfWord rc w =
  map Path.cycleNodeList . concatMap pathesOnPathTree $ pathTreesOfMCycles rc w

spiralReportForWord :: Ord x => LabeledGraphI g x
                                -> g -> RelationCache r x -> [Label] -> [String]
spiralReportForWord gi g rc w = let
    cycles = cyclesOfWord rc w
    putCycle cycle = prettySpiral (LabeledGraph.prettyNode gi g) $ fromHub gi g w cycle
  in [show w] ++ intercalate [""] (map putCycle cycles)

spiralReport :: Ord x => [[Label]] -> LabeledGraphI g x -> g -> [String]
spiralReport words gi g = let
    rc = buildCache (relationTreeRelationCacheableI (genericBitableI gi)) g
    wordStrings = map (spiralReportForWord gi g rc) words
  in intercalate ["", "+++++++++++++++++++", ""] wordStrings

easySpiralReport :: Ord x => Int -> LabeledGraphI g x -> g -> IO ()
easySpiralReport numWords gi g =
  putStr . unlines $ spiralReport words gi g where
    words = take numWords . filter isBaseWord . tail $ Word.allWords labelsList
