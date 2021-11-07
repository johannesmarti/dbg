module DeterminismProperty (
  Partition,
  discrete,
  isTrivial,
  representative,
  eqClass,
  identify,
  isConstructionDeterministic,
  isWeaklyConstructionDeterministic,
  deterministicPartition,
) where

import Control.Exception
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Graph
import MapGraph

type Partition a = Map.Map a a

discrete :: Set.Set a -> Partition a
discrete = Map.fromSet id

isTrivial :: Eq a => Partition a -> Bool
isTrivial partition = Prelude.all (== firstRepresentative) partition where
  firstRepresentative = snd (Map.findMin partition)

representative :: Ord a => Partition a -> a -> a
representative partition element = assert (element `Map.member` partition) $
  partition Map.! element

eqClass :: Ord a => Partition a -> a -> Set.Set a
eqClass partition elem = Set.filter sameClass (Map.keysSet partition) where
  repr = representative partition elem
  sameClass c = representative partition c == repr

identify :: Ord a => Partition a -> a -> a -> Partition a
identify partition x y = let
  repx = representative partition x
  repy = representative partition y
  modifier v = if v == repy then repx else v
    in if repx == repy
         then partition
         else Map.map modifier partition

overlappingPairs :: (Ord x, Ord y) => GraphI g x -> g -> (x -> y) -> [(x,x)]
overlappingPairs gi g f = let
    pairs = ps (Set.toList (domain gi g))
    ps [] = []
    ps (first:others) = Prelude.map (\o -> (first,o)) others ++ ps others
    overlappingConstruction x y = f x /= f y && all (overlapOnLabel x y) labels
    overlapOnLabel x y l = not . Set.null $ Set.map f (predecessors gi g l x) `Set.intersection` Set.map f (predecessors gi g l y)
  in Prelude.filter (uncurry overlappingConstruction) pairs

isConstructionDeterministic :: Ord x => GraphI g x -> g -> Bool
isConstructionDeterministic gi g = Prelude.null $ overlappingPairs gi g id

isWeaklyConstructionDeterministic :: Ord x => GraphI g x -> g -> Bool
isWeaklyConstructionDeterministic gi g = not . isTrivial $
  deterministicPartition gi g

deterministicPartition :: Ord x => GraphI g x -> g -> Partition x
deterministicPartition gi g = inner (discrete (domain gi g)) where
  inner partition = let
    overlappings = overlappingPairs gi g (representative partition)
    coarserPartition = foldl (\p (x,y) -> identify p x y) partition overlappings
      in if Prelude.null overlappings
           then partition
           else inner coarserPartition
