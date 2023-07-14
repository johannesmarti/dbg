module DeterminismProperty (
  Partition,
  discrete,
  isTrivial,
  representative,
  eqClass,
  identify,
  isStrictlyConstructionDeterministic,
  isStronglyConstructionDeterministic,
  deterministicPartition,
  isConstructionDeterministic,
  deterministicAntichain,
  Antichain,
  singletonChain,
  isTotal,
  covers,
  addProposition,
) where

import Control.Exception
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.FiniteFunction hiding (domain)
import LabeledGraphInterface

type Partition a = Function a a

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

overlappingPairs :: (Ord x, Ord y) => LabeledGraphInterface g x -> g -> (x -> y) -> [(x,x)]
overlappingPairs gi g f = let
    pairs = ps (Set.toList (domain gi g))
    ps [] = []
    ps (first:others) = Prelude.map (\o -> (first,o)) others ++ ps others
    overlappingConstruction x y = f x /= f y && all (overlapOnLabel x y) labels
    overlapOnLabel x y l = not . Set.null $ Set.map f (predecessors gi g l x) `Set.intersection` Set.map f (predecessors gi g l y)
  in Prelude.filter (uncurry overlappingConstruction) pairs

isStrictlyConstructionDeterministic :: Ord x => LabeledGraphInterface g x -> g -> Bool
isStrictlyConstructionDeterministic gi g = Prelude.null $ overlappingPairs gi g id

isStronglyConstructionDeterministic :: Ord x => LabeledGraphInterface g x -> g -> Bool
isStronglyConstructionDeterministic gi g = not . isTrivial $
  deterministicPartition gi g

deterministicPartition :: Ord x => LabeledGraphInterface g x -> g -> Partition x
deterministicPartition gi g = inner (discrete (domain gi g)) where
  inner partition = let
    overlappings = overlappingPairs gi g (representative partition)
    coarserPartition = foldl (\p (x,y) -> identify p x y) partition overlappings
      in if Prelude.null overlappings
           then partition
           else inner coarserPartition

type Antichain x = Set.Set (Set.Set x)

singletonChain :: Ord x => Set.Set x -> Antichain x
singletonChain dom = assert (isRealAntichain res) $
                     assert (coversAll dom res) $ res where
  res = Set.map (Set.singleton) dom

isRealAntichain :: Ord x => Antichain x -> Bool
isRealAntichain fam = all (\(x,y) -> not (x `Set.isProperSubsetOf` y))
                          (Set.cartesianProduct fam fam)

coversAll :: Ord x => Set.Set x -> Antichain x -> Bool
coversAll dom antichain =
  all (\elem -> any (\set -> elem `Set.member` set) antichain) dom

isTotal :: Ord x => Set.Set x -> Antichain x -> Bool
isTotal dom ac = assert (isRealAntichain ac) $
                 assert (coversAll dom ac) $ dom `Set.member` ac

covers :: Ord x => Antichain x -> Set.Set x -> Bool
covers antichain prop = assert (isRealAntichain antichain) $
  any (\elem -> prop `Set.isSubsetOf` elem) antichain

addProposition :: Ord x => Set.Set x -> Antichain x -> Antichain x
addProposition prop antichain = assert (isRealAntichain antichain) $
                                assert (isRealAntichain res) $ res where
  filtered = Set.filter (\elem -> not (elem `Set.isSubsetOf` prop)) antichain
  res = if antichain `covers` prop
          then antichain
          else Set.insert prop filtered

isConstructionDeterministic :: Ord x => LabeledGraphInterface g x -> g -> Bool
isConstructionDeterministic gi g = not . (isTotal  (domain gi g)) $
  deterministicAntichain gi g

image :: Ord x => Set.Set x -> (x -> Set.Set x) -> Set.Set x
image set rel = foldl Set.union Set.empty listOfSets where
  list = Set.toList set
  listOfSets = map rel list

deterministicAntichain :: Ord x => LabeledGraphInterface g x -> g -> Antichain x
deterministicAntichain gi g = inner (singletonChain (domain gi g)) where
  {- This code is somehow not perfect. We first filter out all the propositions that are covered in order to be able to detect later whether anything has added. But then if we add the propositions that are not covered the addProposition function still check again whether they are covered (by the somewhat bigger antichain where we now have all the previous new propostions already added. -}
  inner ac = let
    asList = Set.toList ac
    allPairs = [(x,y) | x <- asList, y <- asList]
    newProps = map forward allPairs
    forward (pzero,pone) = zImage `Set.intersection` oImage where
        zImage = image pzero (successors gi g Zero)
        oImage = image pone (successors gi g One)
    propsToAdd = filter (\prop -> not (ac `covers` prop)) newProps
    betterChain = foldl (flip addProposition) ac propsToAdd
      in if Prelude.null propsToAdd
           then ac
           else inner betterChain
