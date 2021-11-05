module DeterminismProperty (
  Partition,
  discrete,
  isTrivial,
  representative,
  eqClass,
  identify,
  isConstructionDeterministic,
  hasDeterminismProperty,
  determinismPartition,
) where

import Control.Exception
import Data.Map.Strict as Map
import Data.Set as Set

import Debug.Trace

import Graph

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

isConstructionDeterministic :: Ord x => GraphI g x -> g -> Bool
isConstructionDeterministic gi g = Prelude.null $ overlappingPairs gi g

isWeaklyConstructionDeterministic :: Ord x => GraphI g x -> g -> Bool
isWeaklyConstructionDeterministic gi g = undefined

--hasDeterminismProperty :: Ord x => GraphI g x -> g -> Set x -> Bool
hasDeterminismProperty gi g set = case determinismPartition gi g set of
  Just _  -> True
  Nothing -> False

--determinismPartition :: Ord x => GraphI g x -> g -> Set x -> Maybe (Partition x)
determinismPartition gi g set = assert (set `isSubsetOf` (domain gi g)) $
  updatePartition pairs (discrete set) where
    pairs = ps (Set.toList set)
    ps [] = []
    ps (first:others) = Prelude.map (\o -> (first,o)) others ++ ps others
    updatePartition [] partition = Just partition
    updatePartition ((x,y):rest) partition =
      if representative partition x == representative partition y
          || not (overlappingConstruction x y)
        then trace ("not overlapping " ++ show x ++ " " ++ show y)  $ updatePartition rest partition
      else if trace ("overlapping " ++ show x ++ " " ++ show y ++ " idfiable?") $ identifiable x y partition
        then trace ("yes") $ let newPartition = identify partition x y
               in if isTrivial newPartition
                    then Nothing
                    else updatePartition rest newPartition
      else trace ("no") $ Nothing
    overlappingConstruction x y = all (overlapOnLabel x y) labels
    overlapOnLabel x y l = not . Set.null $ predecessors gi g l x `Set.intersection` predecessors gi g l y `Set.intersection` set
    identifiable x y par = all (\node -> (all (ifableOnNodeLabel par x y node)) labels) set
    ifableOnNodeLabel par x y node l = let
      preds = predecessors gi g l node
      xClass = eqClass par x
      yClass = eqClass par y
      in (xClass `Set.disjoint` preds && yClass `Set.disjoint` preds)
           || (xClass `isSubsetOf` preds && yClass `isSubsetOf` preds)

determinismPartition' :: Ord x => GraphI g x -> g -> Partition x
determinismPartition' gi g = let
  overlappings = overlappingPairs gi g
    in undefined

