module Search (
  searchDbgHomo,
  homoLargerThan,
) where

import ArcCons
import DeBruijn
import Graph
import Homo

data LargeNumber = IsNumber Int | LargerThan Int

instance Show LargeNumber where
  show (IsNumber n)   = show n
  show (LargerThan n) = show n ++ "<"

searchDbgHomo :: Ord x => GraphI g x -> Int -> g -> LargeNumber
searchDbgHomo gi cutoff graph = worker 1 where
  worker i = if i > cutoff
               then LargerThan cutoff
             else if noHomo (arcConsHomos dbgI gi) (dbg i) graph
               then worker (i + 1)
             else IsNumber i

homoLargerThan :: Ord x => GraphI g x -> Int -> Int -> g -> Bool
homoLargerThan gi cutoff n graph =
  case searchDbgHomo gi cutoff graph of
    IsNumber m   -> m > n
    LargerThan n -> False
