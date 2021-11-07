module BitGraph (
  BitGraph,
  bitGraph,
) where

import Data.Bits
import qualified Data.Set as Set

import UnlabeledBitGraph
import Graph

data BitGraph x = BitGraph {
  size          :: Int,
  zeroBits      :: UnlabeledBitGraph,
  oneBits       :: UnlabeledBitGraph,
}

bitGraphI :: GraphI (BitGraphWrapper x) x
bitGraphI size = GraphI (dom size) (succs size) (preds size)

dom :: Size -> Word -> Set.Set Int
dom size bitset = assert (enoughBits size) $

bitsOfLabel :: BitGraph x -> Label -> UnlabeledBitGraph
bitsOfLabel bg Zero = zeroBits
bitsOfLabel bg One  = oneBits

succs :: Size -> Word -> MapFunction Int
succs size bitset label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasArc size bitset (node,label,v)) (nodes size)

preds :: Size -> Word -> MapFunction Int
preds size bitset label node = assert (isNode size node) $
  Set.fromList $ filter (\v -> hasArc size bitset (v,label,node)) (nodes size)

