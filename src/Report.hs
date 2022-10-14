module Report (
  pathReport, easyPathReport,
  wordReport, easyWordReport,
) where

import qualified Data.Set as Set
import Data.List (maximumBy,intercalate,intersperse)

import Bitable
import LWrappedGraph
import qualified WrappedGraph as WG
import qualified Path
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

pathReport :: Ord x => LabeledGraphI g x -> g -> [String]
pathReport gi g = let
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
     
easyPathReport :: Ord x => LabeledGraphI g x -> g -> IO ()
easyPathReport gi g = putStr . unlines $ pathReport gi g

wordReport :: Ord x => Int -> LabeledGraphI g x -> g -> [String]
wordReport numWords gi g = let
    bf = (genericBitableI gi) g
    s = numBits bf
    cg = rightCayleyGraph bf
    c = Bitable.coding bf
    dec = decode c
    printRel r = Graph.prettyGraph (relationI bf) r
    rc = relationCache bf cg
    wordToRel = relationOfWord rc
    printNode = LabeledGraph.prettyNode gi g
    printRelWithCode r = (printNodeWithSuccs cg r ++ ":") : (printRel r)
    printWordWithRel (w,r) = [show w ++ ":"] ++ printRelWithCode r
    printWordWithRelAndPathes (w,r) = let
        cycles = concatMap pathesOnPathTree $
                   pathTreesOfMCycles rc w
      in printWordWithRel (w,r) ++ map (Path.prettyPath printNode) cycles
    wfs = wellfoundedElements cg
    nwfs = nonWellfoundedElements cg
    finWords = map fst $ finiteWords s cg
    longestFinWord = maximumBy (\a b -> compare (length a) (length b)) finWords
    wordRels = take numWords $ allWords s cg
  in ["About the Cayley graph of the pattern:"] ++
      LabeledGraph.prettyLabeledGraph gi g ++
     ["It has " ++ show (Set.size wfs) ++ " finite and " ++
                   show (Set.size nwfs) ++ " infinite elements.", "",
      "It " ++ (if isConstructionDeterministic gi g then "is" else "is not") ++ " construction deterministic.", "",
      "It " ++ (if pathCondition s cg then "satisfies" else "does not satisfy") ++ " the path condition.", ""] ++ 
--      "It's finite words are:", show finWords,
--      "Of which one with maximal length is " ++ show longestFinWord ++ ".", ""] ++
     ["The relations of the first " ++ show numWords ++ " words are:"] ++
      --intercalate [""] (map printWordWithRel wordRels)
      intercalate [""] (map printWordWithRelAndPathes wordRels)

easyWordReport :: Ord x => Int -> LabeledGraphI g x -> g -> IO ()
easyWordReport numWords gi g = putStr . unlines $ (wordReport numWords gi g)

cyclesOfWord :: Ord x => RelationCache r x -> [Label] -> [[x]]
cyclesOfWord rc w =
  map Path.cycleNodeList . concatMap pathesOnPathTree $ pathTreesOfMCycles rc w

spiralReportForWord :: (Ord x, Show x) => LabeledGraphI g x -> g -> RelationCache r x -> [Label] -> [String]
spiralReportForWord gi g rc w = let
    cycles = cyclesOfWord rc w
    putCycle cycle = show $ fromHub gi g w cycle
  in [show w] ++
       intersperse "" (map putCycle cycles)
       --intercalate [""] (putCycle cycles)
