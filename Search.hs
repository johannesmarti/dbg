module Search (
  searchForGraph,
) where

import ArcCons
import DeBruijn
import Graph
import Homo

data LargeNumber = IsNumber Int | LargerThan Int

instance Show LargeNumber where
  show (IsNumber n)   = show n
  show (LargerThan n) = show n ++ "<"

searchForGraph :: Ord x => Int -> Graph x -> LargeNumber
searchForGraph cutoff graph = worker 1 where
  worker i = if i > cutoff
               then LargerThan cutoff
             else if noHomo arcConsHomos (deBruijnGraph i) graph
               then worker (i + 1)
             else IsNumber i
