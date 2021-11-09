module ConciseSubGraph (
  ConciseSubGraph,
  subdomain,
  conciseSubGraphI,
  fromSubset,
  ConciseSubGraph.caleyGraph,
  ConciseSubGraph.showem,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

import CaleyGraph
import ConciseGraph
import Graph

data ConciseSubGraph = ConciseSubGraph {
  baseGraph :: ConciseGraph,
  subdomain :: [Node]
} deriving Show

conciseSubGraphI :: Size -> GraphI ConciseSubGraph Int
conciseSubGraphI size = GraphI (Set.fromList . subdomain) (succs size) (preds size)

succs :: Size -> ConciseSubGraph -> MapFunction Node
succs size csg label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasArc size (baseGraph csg) (node,label,v)) (subdomain csg)

preds :: Size -> ConciseSubGraph -> MapFunction Node
preds size csg label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasArc size (baseGraph csg) (v,label,node)) (subdomain csg)

fromSubset :: Size -> ConciseGraph -> Set.Set Node -> ConciseSubGraph
fromSubset size superGraph subset = assert (all (< size) subset) $ ConciseSubGraph bg dom where
  dom = Set.toList subset
  bg = superGraph .&. bitsOfInternalArcs
  bitsOfInternalArcs = bitsOfInternalInRel .|. (shiftL bitsOfInternalInRel (size * size))
  bitsOfSuccsInSubset = listToBitmask dom
  bitsOfInternalInRel = foldl (\accum n -> accum .|. (shiftL bitsOfSuccsInSubset (n * size))) 0 dom

listToBitmask :: [Node] -> ConciseGraph
listToBitmask = foldl setBit 0

caleyGraph :: Size -> ConciseSubGraph -> CaleyGraph
caleyGraph size = (ConciseGraph.caleyGraph size) . baseGraph

showem :: Size -> ConciseSubGraph -> String
showem size graph = unlines $ prettyGraph (conciseSubGraphI size) show graph
