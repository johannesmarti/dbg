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

import BitableInterface
import BitGraph
import Coding
import GraphInterface
import Data.Label
import RelationTree
import WordTree (labelOfWord)

data RelationCache r x = RelationCache {
  outputType :: GraphInterface r x,
  reflexivesUniversalInMultiple :: r -> Set x,
  relationOfWord :: [Label] -> r
}

type RelationCachableInterface g x r = g -> RelationCache r x

buildCache :: RelationCachableInterface g x r -> g -> RelationCache r x
buildCache ci = ci

relationTreeRelationCacheableInterface :: BitableInterface g x -> RelationCachableInterface g x BitGraph
relationTreeRelationCacheableInterface bi g = RelationCache bgi rum rw where
  Bitification s lbg c bgi = bi g
  rt = relationTree (lbg,s)
  rum = decodeSet c . BitGraph.reflexivesUnivInMultiple s
  rw = labelOfWord rt
