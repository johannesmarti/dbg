module LiftedGraphReport (
  liftedGraphRelReport,
  easyLiftedGraphRelReport,
) where

import Control.Exception.Base
import qualified Data.Set as Set
import Data.List (intersperse)

import Bitable
import Graph
import Label
import LiftedGraph
import PathTree
import RelationCache
import qualified LabeledGraph

liftedGraphRelReport :: LiftedGraph x -> [Label] -> [String]
liftedGraphRelReport lg w = let
    relationCache = buildCache (relationTreeRelationCacheableI liftedGraphBitableI) lg
    relOfWord = relationOfWord relationCache

    ig = graph lg
    cans = filter (weakDominationFilter ig) (liftableCandidates ig)

    printRel = prettyGraph (outputType relationCache)
    r = relOfWord w
    pts :: [PathTree Int]
    pts = pathTreesOfMCycles relationCache w
    cycleWithGoodCansPrinter :: [LiftingCandidate] -> PathTree Int -> String -> [String]
    cycleWithGoodCansPrinter goodCans (There t) s = [((show t ++) . (" " ++) . (show (map extractPair goodCans) ++)) s]
    cycleWithGoodCansPrinter cansSoFar (Step n l succs) str = let
        concatter :: PathTree Int -> [String]
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
    fancyPrinter :: PathTree Int -> [String]
    fancyPrinter pt = cycleWithGoodCansPrinter [] pt ""
  in [show w ++ ":"] ++ printRel r ++
        intersperse "" (concatMap fancyPrinter pts)

easyLiftedGraphRelReport :: LiftedGraph x -> [Label] -> IO ()
easyLiftedGraphRelReport lg w = putStr . unlines $ liftedGraphRelReport lg w
