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
  hasDeterminismProperty,
) where

import Control.Exception
import Data.Map.Strict as Map
import Data.Set as Set

import Debug.Trace

import Graph
import MapGraph

type Partition a = Map a a

discrete :: Set a -> Partition a
discrete = fromSet id

isTrivial :: Eq a => Partition a -> Bool
isTrivial partition = Prelude.all (== firstRepresentative) partition where
  firstRepresentative = snd (Map.findMin partition)

representative :: Ord a => Partition a -> a -> a
representative partition element = assert (element `Map.member` partition) $
  partition ! element

eqClass :: Ord a => Partition a -> a -> Set a
eqClass partition elem = Set.filter sameClass (keysSet partition) where
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

overlappingPairs :: Ord x => GraphI g x -> g -> [(x,x)]
overlappingPairs gi g = let
    pairs = ps (Set.toList (domain gi g))
    ps [] = []
    ps (first:others) = Prelude.map (\o -> (first,o)) others ++ ps others
    overlappingConstruction x y = all (overlapOnLabel x y) labels
    overlapOnLabel x y l = not . Set.null $ predecessors gi g l x `Set.intersection` predecessors gi g l y
  in Prelude.filter (uncurry overlappingConstruction) pairs

--isConstructionDeterministic :: Ord x => GraphI g x -> g -> Bool
isConstructionDeterministic gi g = Prelude.null $ overlappingPairs gi g

--isWeaklyConstructionDeterministic :: Ord x => GraphI g x -> g -> Bool
isWeaklyConstructionDeterministic gi g = not . isTrivial $
  deterministicPartition gi g

--deterministicPartition :: Ord x => GraphI g x -> g -> Partition x
deterministicPartition gi g = inner (discrete (domain gi g)) where
  inner partition = let
    quotient = MapGraph.projection gi g (representative partition)
    overlappings = overlappingPairs mapGraphI quotient
    keepIdentifying [] p = p
    keepIdentifying ((r1,r2):rest) p =
      if trace ("\nquotient " ++ show quotient ++ "overlappings " ++ show overlappings) $ r1 == r2
        then keepIdentifying rest p
        else let newPartition = identify partition r1 r2
                 updatedRest = Prelude.map (fmap (representative newPartition)) rest
             in keepIdentifying updatedRest newPartition
    coarserPartition = keepIdentifying overlappings partition
      in if Prelude.null overlappings
           then partition
           else inner coarserPartition

--hasDeterminismProperty :: Ord x => GraphI g x -> g -> Set x -> Bool
hasDeterminismProperty gi g set =
  isWeaklyConstructionDeterministic mapGraphI (subgraph gi g set)
