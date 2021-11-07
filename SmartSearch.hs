module SmartSearch (
  homoLargerThan,
) where

import Data.Set as Set
import Data.Map.Strict as Map

import ArcCons
import BitGraph
import CaleyGraph
import DeBruijn
import Graph
import Homo
import DeterminismProperty

import Debug.Trace

data LargeNumber = IsNumber Int | LargerThan Int

instance Show LargeNumber where
  show (IsNumber n)   = show n
  show (LargerThan n) = show n ++ "<"

noHomos :: Int -> Int -> BitGraph -> CaleyGraph -> Set.Set (Set.Set Int) -> Bool
noHomos size i graph cg candidates = all check candidates where
  deBruijnGraph = dbg i
  computeApprox candi = Map.fromSet f (domain dbgI deBruijnGraph) where
        f dbgnode = Set.filter (isPossibleValue size cg (nodeToList i dbgnode) candi) candi
  check candi = let approx = computeApprox candi
          in if isPossible approx
               then noHomo (arcConsHomosFromApprox dbgI (bitGraphI size) approx) (dbg i) graph
               else True

searchDbgHomo :: Int -> Int -> BitGraph -> CaleyGraph -> Set.Set (Set.Set Int) -> LargeNumber
searchDbgHomo size cutoff graph cg candidates = worker 1 where
  worker i = if i > cutoff
               then LargerThan cutoff
             else if noHomos size i graph cg candidates
               then (if i == 6 then trace (show graph ++ " Six! " ++ show candidates) $ worker (i + 1) else worker (i + 1))
             else (if i >= 5 then trace (show graph ++ " at " ++ show i ++ " " ++ show candidates) IsNumber i else IsNumber i)

homoLargerThan :: Int -> Int -> Int -> Word -> Bool
homoLargerThan size cutoff n graph = let
    cg = rightCaleyGraph size graph
    allNodes = Set.fromList (nodes size)
    subsets = Set.powerSet allNodes
    candidates = Set.filter (\s -> setIsGood size cg s && not (hasDeterminismProperty (bitGraphI size) graph s)) subsets
  in if reflexivityCondition size cg && not (Set.null candidates)
       then case searchDbgHomo size cutoff graph cg candidates of
              IsNumber m   -> m > n
              LargerThan n -> False
       else False
