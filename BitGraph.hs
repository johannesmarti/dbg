module BitGraph (
  BitGraph(..),
  Node,
  bitGraphI,
) where

import Data.Bits
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ConciseGraph hiding (Node, Size, nodes)
import Graph
import UnlabeledBitGraph

data BitGraph = BitGraph {
  size       :: Int,
  zeroBitMap :: UnlabeledBitGraph,
  oneBitMap  :: UnlabeledBitGraph
}

fromConciseGraph :: Size -> ConciseGraph -> BitGraph
fromConciseGraph size cg = BitGraph size (relationOfLabel size cg Zero)
                                         (relationOfLabel size cg One)

bitsOfLabel :: BitGraph -> Label -> UnlabeledBitGraph
bitsOfLabel bg Zero = zeroBitMap bg
bitsOfLabel bg One  = oneBitMap bg

bitGraphI :: GraphI BitGraph Node
bitGraphI = GraphI dom succs preds

dom :: BitGraph -> Set.Set Node
dom bg = Set.fromList $ nodes (size bg)

succs :: BitGraph -> MapFunction Node
succs bg label node = Set.fromList $ succsAsList (size bg) (bitsOfLabel bg label) node

preds :: BitGraph -> MapFunction Node
preds bg label node = Set.fromList $ succsAsList (size bg) (bitsOfLabel bg label) node
