module RelationCache (
  RelationCachableI,
  RelationCache(..),
  outputType,
  RelationCache.reflexivesUniversalInMultiple,
  relationOfWord,
  buildCache,
  relationTreeRelationCacheableI,
) where

import Data.Set

import Bitable
import BitGraph
import Coding
import Graph
import Label
import RelationTree
import WordTree (labelOfWord)

data RelationCache r x = RelationCache {
  outputType :: GraphI r x,
  reflexivesUniversalInMultiple :: r -> Set x,
  relationOfWord :: [Label] -> r
}

type RelationCachableI g x r = g -> RelationCache r x

buildCache :: RelationCachableI g x r -> g -> RelationCache r x
buildCache ci = ci

relationTreeRelationCacheableI :: BitableI g x -> RelationCachableI g x BitGraph
relationTreeRelationCacheableI bi g = RelationCache bgi rum rw where
  Bitification s lbg c bgi = bi g
  rt = relationTree (lbg,s)
  rum = decodeSet c . BitGraph.reflexivesUnivInMultiple s
  rw = labelOfWord rt
