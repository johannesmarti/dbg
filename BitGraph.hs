module BitGraph (
  BitGraph,
  bitGraphI,
) where

-- WORK IN PROGRESS! --

import Data.Bits
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import UnlabeledBitGraph
import Graph

data BitGraph x = BitGraph {
  size       :: Int,
  zeroBitMap :: UnlabeledBitGraph,
  oneBitMap  :: UnlabeledBitGraph,
  encoder    :: Map.Map x Node,
  decoder    :: Map.Map Node x
}

bitsOfLabel :: BitGraph x -> Label -> UnlabeledBitGraph
bitsOfLabel bg Zero = zeroBitMap bg
bitsOfLabel bg One  = oneBitMap bg

bitGraphI :: Ord x => GraphI (BitGraph x) x
bitGraphI = GraphI dom succs preds

bitGraphRawI :: GraphI (BitGraph x) Node 
bitGraphRawI = undefined

dom :: Ord x => BitGraph x -> Set.Set x
dom bitgraph = Map.keysSet (encoder bitgraph)

succs :: Ord x => BitGraph x -> MapFunction x
succs bg label node = Set.fromList . (map (aggressiveDecode bg)) $ succsAsList (size bg) (bitsOfLabel bg label) (aggressiveEncode bg node)

preds :: Ord x => BitGraph x -> MapFunction x
preds bg label node = Set.fromList . (map (aggressiveDecode bg)) $ predsAsList (size bg) (bitsOfLabel bg label) (aggressiveEncode bg node)

encode :: Ord x => BitGraph x -> x -> Maybe Node
encode bg = undefined

decode :: Ord x => BitGraph x -> Node -> Maybe x
decode bg = undefined

aggressiveEncode :: Ord x => BitGraph x -> x -> Node
aggressiveEncode bg v =
  Map.findWithDefault (error "node not in domain") v (encoder bg) 

aggressiveDecode :: Ord x => BitGraph x -> Node -> x
aggressiveDecode bg n =
  Map.findWithDefault (error "number not an index") n (decoder bg) 
{-

-- Should Maybe stay on top of sets?
linearization :: Ord a => ParityGame a -> Linearization a
linearization game = Linearization 
  linGame
  encoder
  decoder where
    linGame = linearFromGraph smallerLessImportant
                (applyIso (fromJust . encoder) graph)
                ((toMove game) . fromJust . decoder)
    graph = gameGraph game
    dom = Set.toList (domain graph)
    --graph = graphOnDomain dom priority
    (noIters,iters) = partition (isNothing . (canIterate game)) dom
    unranked = zip noIters [0..]
    itersOrderedByIncreasingPriority = sortBy (lessImportantFirst (ranking game)) iters
    liftedPriorities = liftPrioritiesTo (winsIteration game) (length noIters) itersOrderedByIncreasingPriority
    reAssoc = unranked ++ liftedPriorities
    newDom = map snd reAssoc
    enMap = Map.fromList reAssoc
    deMap = Map.fromList (map swap reAssoc)
    encoder obj = Map.lookup obj enMap
    decoder n = Map.lookup n deMap


-}

