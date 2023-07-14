module Report (
  cayleyReport, easyCayleyReport,
  spiralReport, easySpiralReport,
) where

import qualified Data.Set as Set
import Data.List (maximumBy,intercalate)

import Word
import Data.Label
import BitableInterface
import qualified Data.Path as Path
import LabeledGraphInterface
import GraphInterface as GI
import CayleyGraph hiding (relationOfWord)
import Coding
import RelationCache
import DeterminismProperty
import PathTree
import Spiral


cayleyReport :: Ord x => LabeledGraphInterface g x -> g -> [String]
cayleyReport gi g = let
    bf = (genericBitableInterface gi) g
    s = numBits bf
    inner = labeledBitGraph bf
    cg = rightCayleyGraph bf
    c = BitableInterface.coding bf
    dec = decode c
    printRel r = GI.prettyGraph (relationI bf) r
    printRelWithCode r = (printNodeWithSuccs cg r ++ ":") : (printRel r)
    wfs = wellfoundedElements cg
    nwfs = nonWellfoundedElements cg
    finWords = finiteWords s cg
    (longestFinWord,relOfLongest) = maximumBy (\(a,_) (b,_) -> compare (length a) (length b)) finWords
  in ["About the Cayley graph of the pattern:"] ++
      LabeledGraphInterface.prettyLabeledGraph gi g ++
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
     
easyCayleyReport :: Ord x => LabeledGraphInterface g x -> g -> IO ()
easyCayleyReport gi g = putStr . unlines $ cayleyReport gi g

cyclesOfWord :: Ord x => RelationCache r x -> [Label] -> [[x]]
cyclesOfWord rc w =
  map Path.cycleNodeList . concatMap pathesOnPathTree $ pathTreesOfMCycles rc w

spiralReportForWord :: Ord x => LabeledGraphInterface g x
                                -> g -> RelationCache r x -> [Label] -> [String]
spiralReportForWord gi g rc w = let
    cycles = cyclesOfWord rc w
    putCycle cycle = prettySpiral (LabeledGraphInterface.prettyNode gi g) $ fromHub gi g w cycle
  in [show w] ++ intercalate [""] (map putCycle cycles)

spiralReport :: Ord x => [[Label]] -> LabeledGraphInterface g x -> g -> [String]
spiralReport words gi g = let
    rc = buildCache (relationTreeRelationCacheableInterface (genericBitableInterface gi)) g
    wordStrings = map (spiralReportForWord gi g rc) words
  in intercalate ["", "+++++++++++++++++++", ""] wordStrings

easySpiralReport :: Ord x => Int -> LabeledGraphInterface g x -> g -> IO ()
easySpiralReport numWords gi g =
  putStr . unlines $ spiralReport words gi g where
    words = take numWords . filter isBaseWord . tail $ Word.allWords labelsList
