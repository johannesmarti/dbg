module Plans.Plan (
  CoveringGraphAnnotation,
  Plan,
  empty,
  Plans.Plan.insert,
  Plans.Plan.lookup,
  Plans.Plan.forceLookup,
  annotated,
) where

import Data.Maybe (fromMaybe)

import qualified Data.WordMaps.Algebraic as WordMap
import Data.Label
import Plans.Spoke
import Plans.CoveringGraph

{-
 Maybe this module should be called CoveringGraphAnnotation or
CoveringGraphLabeling. But then Plans are probabely the most used kind of
CoveringGraphAnnotations.
-}

type CoveringGraphAnnotation x = WordMap.WordMap x
type Plan x = CoveringGraphAnnotation (Spoke x)

empty :: CoveringGraphAnnotation x
empty = WordMap.empty

insert :: CoveringNode -> x -> CoveringGraphAnnotation x
                            -> CoveringGraphAnnotation x
insert cn s p = WordMap.insert (address cn) s p

lookup :: CoveringNode -> CoveringGraphAnnotation x -> Maybe x
lookup cn p = WordMap.lookup (address cn) p

forceLookup :: CoveringNode -> CoveringGraphAnnotation x -> x
forceLookup cn p = fromMaybe err (Plans.Plan.lookup cn p) where
  err = error $ "covering node " ++ prettyWord (turningWord cn) ++ " not in CoveringGraphAnnotation"

annotated :: CoveringNode -> CoveringGraphAnnotation x -> Bool
annotated cn p = WordMap.inDomain (address cn) p
