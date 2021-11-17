module BitGraph (
  BitGraph,
  size,
  zeroBitMap,
  oneBitMap,
  fromConciseGraph,
  toConciseGraph,
  Node,
  bitGraphI,
  fromArcs,
  caleyGraph,
) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import CaleyGraph
import ConciseGraph (ConciseGraph,relationOfLabel,enoughBits)
import Graph
import UnlabeledBitGraph

data BitGraph = BitGraph {
  size       :: Int,
  zeroBitMap :: UnlabeledBitGraph,
  oneBitMap  :: UnlabeledBitGraph
}

fromConciseGraph :: Size -> ConciseGraph -> BitGraph
fromConciseGraph s cg = BitGraph s (relationOfLabel s cg Zero)
                                   (relationOfLabel s cg One)

toConciseGraph :: BitGraph -> (ConciseGraph, Size)
toConciseGraph bg = let s = size bg
                        cg = (zeroBitMap bg) .|. shiftL (oneBitMap bg) (s * s)
  in assert (ConciseGraph.enoughBits s) $ (cg,s)

fromArcs :: Size -> [Arc Node] -> BitGraph
fromArcs size arcs = BitGraph size zbm obm where
  (zbm, obm) = foldl setLabeledArc (nullWord, nullWord) arcs
  setLabeledArc (zeroWord,oneWord) (u,Zero,v) = (setArc size zeroWord (u,v), oneWord)
  setLabeledArc (zeroWord,oneWord) (u,One ,v) = (zeroWord, setArc size oneWord (u,v))

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
preds bg label node = Set.fromList $ predsAsList (size bg) (bitsOfLabel bg label) node

caleyGraph :: BitGraph -> CaleyGraph
caleyGraph graph  = rightCaleyGraph (size graph)
                                    (zeroBitMap graph, oneBitMap graph)

