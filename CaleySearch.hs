module CaleySearch (
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
searchDbgHomo size cutoff graph cg candidates = trace (show graph ++ " " ++ show candidates) $ worker 1 where
  worker i = trace (show i) $ if i > cutoff
               then LargerThan cutoff
             else if noHomos size i graph cg candidates
               then worker (i + 1)
             else IsNumber i

homoLargerThan :: Int -> Int -> Int -> Word -> Bool
homoLargerThan size cutoff n graph = let
    cg = rightCaleyGraph size graph
    allNodes = Set.fromList (nodes size)
    subsets = Set.powerSet allNodes
    candidates = Set.filter (setIsGood size cg) subsets
  in if reflexivityCondition size cg && not (Set.null candidates)
       then case searchDbgHomo size cutoff graph cg candidates of
              IsNumber m   -> m > n
              LargerThan n -> False
       else False
