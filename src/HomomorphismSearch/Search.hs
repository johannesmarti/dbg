module HomomorphismSearch.Search (
  searchDbgHomomorphism,
) where

import HomomorphismSearch.ArcCons
import DeBruijnGraph
import LabeledGraph
import HomomorphismSearch.Homomorphism

data LargeNumber = IsNumber Int | LargerThan Int

instance Show LargeNumber where
  show (IsNumber n)   = show n
  show (LargerThan n) = show n ++ "<"

searchDbgHomomorphism :: Ord x => LabeledGraphI g x -> Int -> g -> LargeNumber
searchDbgHomomorphism gi cutoff graph = worker 1 where
  worker i = if i > cutoff
               then LargerThan cutoff
             else if noHomomorphism (arcConsHomomorphisms dbgI gi) (dbg i) graph
               then worker (i + 1)
             else IsNumber i
