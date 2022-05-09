module LiftedGraphReport (
  liftedGraphReport,
  easyLiftedGraphReport,
) where

import Control.Exception.Base
import qualified Data.Set as Set
import Data.List (maximumBy,intercalate,intersperse)

import Graph
import Label
import LiftedGraph
import PairGraph
import BitGraph
import RelationTree
import PathTree
import WordTree
import qualified LabeledGraph

liftedGraphReport :: LiftedGraph x -> [String]
liftedGraphReport lg = let
    (lbg,s) = LiftedGraph.toLBitGraph lg
    rt = relationTree (lbg,s)
    relOfWord = labelOfWord rt

    --wordRelList = tail $ allWordsWithout rt (hasUniv s)
    wordRelList = tail $ take 31 $ allWordsWithout rt (\_ -> False)

    --onCycles = firstArcsOnCycles (lbg, s) wt
    onCycles w = Set.fromList $ arcsOnMCycles (lbg, s) relOfWord w

    ig = graph lg
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)

    printRel r = prettyGraph (bitGraphI s) r
    printWordWithRel (w,r) = let
        arcsOnCycles = onCycles w
        intersectingCans = map extractPair $ filter intersectsCycles cans
        intersects a b = not (a `Set.disjoint` b)
        intersectsCycles can = (Set.fromList $ labeledArcsOfCandidate can)
                                 `intersects` arcsOnCycles
        pts :: [PathTree]
        pts = pathTreesOfMCycles (lbg,s) relOfWord w
        cycleWithGoodCansPrinter :: [LiftingCandidate] -> PathTree -> String -> [String]
        cycleWithGoodCansPrinter goodCans (There t) s = [((show t ++) . (" " ++) . (show (map extractPair goodCans) ++)) s]
        cycleWithGoodCansPrinter cansSoFar (Step n l succs) str = let
            concatter :: PathTree -> [String]
            concatter sut = let
                t = extractNode sut
                arc = (n,l,t)
                nL = case sut of
                        There s -> head w
                        Step _ l' _ -> l'
                tSuccs = LabeledGraph.successors intGraphI ig nL t
                useful c = let
                    (u,v) = extractPair c
                    other = if u == t
                              then v
                              else assert (v == t) $ u
                    otherSuccs = LabeledGraph.successors intGraphI ig nL other
                    givesMore = not (otherSuccs `Set.isSubsetOf` tSuccs)
                  in arc `elem` labeledArcsOfCandidate c && givesMore
                cansToAdd = filter useful cans
                moreCans = cansToAdd ++ cansSoFar
              in cycleWithGoodCansPrinter moreCans sut str
          in (map ((show n ++) . (" " ++) . (labelToSymbol l ++) . ("> " ++))) $ 
               concatMap concatter succs
        fancyPrinter :: PathTree -> [String]
        fancyPrinter pt = cycleWithGoodCansPrinter [] pt ""
      in [show w ++ ":"] ++ printRel (label r) ++
            intersperse "" (concatMap fancyPrinter pts)
              --[show (pathTreesOfMCycles (lbg,s) wt w),
              -- show (Set.toList arcsOnCycles), show (intersectingCans)]
  in intercalate [""] (map printWordWithRel wordRelList)

easyLiftedGraphReport :: LiftedGraph x -> IO ()
easyLiftedGraphReport = putStr . unlines . liftedGraphReport
