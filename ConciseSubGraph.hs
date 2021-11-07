module ConciseSubGraph (
  ConciseSubGraph,
  conciseSubGraphI,
  fromSubset,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

import ConciseGraph
import Graph

data ConciseSubGraph = ConciseSubGraph {
  baseGraph :: ConciseGraph,
  dom       :: [Node]
}

conciseSubGraphI :: Size -> GraphI ConciseSubGraph Int
conciseSubGraphI size = GraphI (Set.fromList . dom) (succs size) (preds size)

succs :: Size -> ConciseSubGraph -> MapFunction Node
succs size csg label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasArc size (baseGraph csg) (node,label,v)) (dom csg)

preds :: Size -> ConciseSubGraph -> MapFunction Node
preds size csg label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasArc size (baseGraph csg) (v,label,node)) (dom csg)

fromSubset :: Size -> ConciseGraph -> Set.Set Node -> ConciseSubGraph
fromSubset size superGraph subset = assert (subset `Set.isSubsetOf` Set.fromList (nodes size)) $ ConciseSubGraph bg dom where
  dom = Set.toList subset
  bg = superGraph .&. bitsOfInternalArcs
  bitsOfInternalArcs = undefined
