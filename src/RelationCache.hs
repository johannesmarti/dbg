module RelationCache (
  RelationCachableInterface,
  RelationCache(..),
  outputType,
  RelationCache.reflexivesUniversalInMultiple,
  relationOfWord,
  buildCache,
  relationTreeRelationCacheableInterface,
) where

import Data.Set

import Bitify.Bitifier
import Graphs.BitGraph
import Bitify.Coding
import Graphs.GraphInterface
import Data.Label
import RelationTree
import Data.WordTree (labelOfWord)

data RelationCache r x = RelationCache {
  outputType :: GraphInterface r x,
  reflexivesUniversalInMultiple :: r -> Set x,
  relationOfWord :: [Label] -> r
}

type RelationCachableInterface g x r = g -> RelationCache r x

buildCache :: RelationCachableInterface g x r -> g -> RelationCache r x
buildCache ci = ci

relationTreeRelationCacheableInterface :: Bitifier g x -> RelationCachableInterface g x BitGraph
relationTreeRelationCacheableInterface bi g = RelationCache bgi rum rw where
  bitification = bi g
  s = numBits bitification
  rt = relationTree (labeledBitGraph bitification, s)
  rum = decodeSet (coding bitification) . Graphs.BitGraph.reflexivesUnivInMultiple s
  rw = labelOfWord rt
  bgi = relationInterface bitification
