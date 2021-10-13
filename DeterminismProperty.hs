module DeterminismProperty (
  Partition,
  hasDeterminismProperty,
) where

import Data.Set

import Graph

type Partition = Int

hasDeterminismProperty :: GraphI g x -> g -> Set x -> Maybe Partition
hasDeterminismProperty = undefined
