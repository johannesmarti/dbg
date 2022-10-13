module RelationCache (

) where

import Data.Set as Set

import Bitify
import BitGraph
import Coding
import ConciseGraph
import CommonLGraphTypes
import Graph
import Label
import qualified LabeledGraph as LG
import LiftedGraph
import RelationTree
import WordTree (labelOfWord)
import LWrappedGraph

data RelationCache r x = RelationCache {
  outputType     :: GraphI r x,
  relationOfWord :: [Label] -> r
}

type RelationCachableI g x r = g -> (RelationCache r x)

buildCache :: RelationCachableI g x r -> g -> RelationCache r x
buildCache ci = ci

lBitGraphRelationCacheI :: Size -> RelationCachableI LBitGraph Node BitGraph
lBitGraphRelationCacheI s lbg = RelationCache (bitGraphI s) rw where
  rt = relationTree (lbg,s)
  rw = labelOfWord rt

liftedGraphRelationCacheI :: RelationCachableI (LiftedGraph x) Int BitGraph
liftedGraphRelationCacheI lg = cache where
  (lbg,s) = LiftedGraph.toLBitGraph lg
  cache = buildCache (lBitGraphRelationCacheI s) lbg

conciseGraphRelationCacheI :: Size -> RelationCachableI
                                        ConciseGraph Node BitGraph
conciseGraphRelationCacheI s cg = cache where
  lbg = ConciseGraph.toLBitGraph s cg
  cache = buildCache (lBitGraphRelationCacheI s) lbg

genericRelationCacheI :: Ord x => LG.LabeledGraphI g x -> RelationCachableI g x BitGraph
genericRelationCacheI gi g = cache where
  (wg,s) = labeledBitify gi g
  c = coding wg
  decodePair (x,y) = (decode c x, decode c y)
  bgCache = lBitGraphRelationCacheI s (innerGraph wg)
  bgi = bitGraphI s
  outTypeI = interfaceFromAll (\_ -> LG.domain (lWrappedGraphI (lBitGraphI s)) wg)
                              (\bg node -> Set.map (decode c) $
                                            successors bgi bg (encode c node))
                              (\bg node -> Set.map (decode c) $
                                            predecessors bgi bg (encode c node))
                              (\bg (x,y) -> hasArc bgi bg (encode c x, encode c y))
                              (\bg -> Prelude.map decodePair $ arcs bgi bg)
                              (\bg node -> prettyOuterNode wg node)
  cache = RelationCache outTypeI (relationOfWord bgCache)
