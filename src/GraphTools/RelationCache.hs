module GraphTools.RelationCache (
  RelationCache(..),
  outputType,
  GraphTools.RelationCache.reflexivesUniversalInMultiple,
  relationOfWord,
  relationTreeCache,
) where

import Data.Set

import Data.Label
import Data.WordTree (labelOfWord)
import Graphs.GraphInterface
import Graphs.BitGraph
import Bitify.Coding (decodeSet)
import Bitify.Bitifier
import GraphTools.RelationTree

data RelationCache r x = RelationCache {
  outputType :: GraphInterface r x,
  reflexivesUniversalInMultiple :: r -> Set x,
  relationOfWord :: [Label] -> r
}

relationTreeCache :: Bitifier g x -> g -> RelationCache BitGraph x
relationTreeCache bi g = RelationCache bgi rum rw where
  bitification = bi g
  s = numBits bitification
  rt = relationTree (labeledBitGraph bitification, s)
  rum = decodeSet (coding bitification) . Graphs.BitGraph.reflexivesUnivInMultiple s
  rw = labelOfWord rt
  bgi = relationInterface bitification
