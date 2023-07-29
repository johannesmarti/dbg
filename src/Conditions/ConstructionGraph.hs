module Conditions.ConstructionGraph (
  powerGraphInterface,
  universalReachability,
  weightMap,
  powerSpheres,
  powerWeightMap,
  bestPowerPredecessors,
  prettyReachability,
  innerSpec,
) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Set.Extra as SE
import Data.List (intercalate, intersperse)
import Test.Hspec

import Data.Label
import Graphs.LabeledGraphInterface

powerGraphInterface :: Ord x => LabeledGraphInterface g x -> LabeledGraphInterface g (Set.Set x)
powerGraphInterface gi = let
    dom g = Set.delete Set.empty (Set.powerSet (domain gi g))
    {- This could be done much more efficiently by computng the biggest successor set and then taking all subsets. -}
    hasA g label (u,v) = all (\y -> any (\x -> hasArc gi g label (x,y)) u) v
    pret g u = "{" ++ intercalate ", " (map (prettyNode gi g) $ Set.toList u)
                   ++ "}"
  in interfaceFromHasArcPretty dom hasA pret

intersects :: Ord a => Set.Set a -> Set.Set a -> Bool
intersects x y = not (Set.disjoint x y)

universalReachability :: Ord x => LabeledGraphInterface g x -> g -> Set.Set x
                                  -> [Set.Set x]
universalReachability gi g base = base : worker base base where
  worker reachable justAdded = let
      zeroCandidates = SE.concatMap (newSuccs Zero) justAdded
      oneCandidates  = SE.concatMap (newSuccs One ) justAdded
      newSuccs l n = successors gi g l n Set.\\ reachable
      zeroToAdd = Set.filter (predOfLabelThere One) zeroCandidates
      oneToAdd  = Set.filter (predOfLabelThere Zero) oneCandidates
      predOfLabelThere l x = (predecessors gi g l x) `intersects` reachable
      added = zeroToAdd `Set.union` oneToAdd
    in if Set.null added
        then []
        else added : worker (reachable `Set.union` added) added

weightMap :: Ord x => [Set.Set x] -> Map.Map x Int
weightMap spheres = worker 0 spheres Map.empty where
  worker _ [] accum = accum
  worker d (nextSphere:rest) accum =
    worker (d + 1) rest (foldl (\m x -> Map.insert x d m) accum nextSphere)

powerSpheres :: Ord x => LabeledGraphInterface g x -> g -> [Set.Set (Set.Set x)]
powerSpheres gi g = universalReachability pgi g singletonSets where
  pgi = powerGraphInterface gi
  singletonSets = Set.map Set.singleton (domain gi g)

powerWeightMap :: Ord x => LabeledGraphInterface g x -> g -> (Set.Set x -> Int)
powerWeightMap gi g = let cache = weightMap (powerSpheres gi g)
  in (\pn -> (cache Map.! pn))

sections :: Ord x => Set.Set (Set.Set x) -> Set.Set (Set.Set x)
sections generators = Set.fromList (worker (Set.toList generators)) where
  worker [] = [Set.empty]
  worker (nextSet:rest) = concatMap (\e -> map (Set.insert e) (worker rest))
                                    (Set.toList nextSet)
  --worker (nextSet:rest) = concatMap (\s -> map (\e -> Set.insert e s) (Set.toList nextSet))
   --                                 (worker rest)
  -- not sure which of these variants is better. It's one of this fucking annoying cases where lazy evaluation makes the computation model of haskell a fucking pain to understand.

minimals :: (x -> Int) -> [x] -> [x]
minimals weight list = worker list maxBound [] where
  worker [] _ accum = accum
  worker (next:rest) currentBest accum = let nextWeight = weight next
    in case compare nextWeight currentBest of
         LT -> worker rest nextWeight [next]
         EQ -> worker rest currentBest (next : accum)
         GT -> worker rest currentBest accum

-- I guess with some thought this could be done more efficiently (so what?)
removeSupersets :: Ord x => [Set.Set x] -> [Set.Set x]
removeSupersets list = filter (not . isDominated) list where
  isDominated set = any (\other -> other `Set.isProperSubsetOf` set) list

bestPowerPredecessors :: Ord x => LabeledGraphInterface g x -> g
                             -> (Set.Set x -> Int) -> Label -> Set.Set x
                             -> [Set.Set x]
bestPowerPredecessors gi g weight l set = let
    setOfPredecessorSets = Set.map (predecessors gi g l) set
    sectionsOverPredecessors = sections setOfPredecessorSets
    minimalSections = minimals weight (Set.toList sectionsOverPredecessors)
    result = removeSupersets minimalSections
  in result

prettyReachability :: (x -> String) -> [Set.Set x] -> [String]
prettyReachability nodePrinter reachabilitySpheres = let
    printSphere set = intercalate ", " (map nodePrinter $ Set.toList set)
  in intersperse "----------------------" $ map printSphere reachabilitySpheres

innerSpec :: Spec
innerSpec = do
  describe "sections" $ do
    let typedEmpty = Set.empty :: Set.Set ()
    it "sections of {{}} are {}" $
      sections (Set.singleton typedEmpty) `shouldBe` Set.empty
    it "sections of {} are singleton {{}}" $
      sections Set.empty `shouldBe` Set.singleton typedEmpty
    let original = [[1,2,3],[3,9],[4,1]]
    let result   = [ [1,3,4],[1,3,1],[1,9,4],[1,9,1],
                     [2,3,4],[2,3,1],[2,9,4],[2,9,1],
                     [3,3,4],[3,3,1],[3,9,4],[3,9,1] ]
    let setify listOfLists = Set.fromList (map Set.fromList listOfLists)
    it "sections on complex example make sense" $
      sections (setify original) `shouldBe` setify result
  describe "minimals" $ do
    it "minimals id [] = []" $
      minimals id [] `shouldBe` []
    it "minimals id [1,2,3] = [1]" $
      minimals id [1,2,3] `shouldBe` [1]
    it "minimals id [3,2,1] = [1]" $
      minimals id [1,2,3] `shouldBe` [1]
    it "minimals (`mod` 2) [3,2,1,4,5,6] = [2,4,6]" $
      Set.fromList (minimals (`mod` 2) [3,2,1,4,5,6])
        `shouldBe` Set.fromList [2,4,6]
  describe "removeSupersets" $ do
    it "removeSupersets [{1,2,4}] = [{1,2,4}]" $
      removeSupersets [Set.fromList [1,2,4]] `shouldBe` [Set.fromList [1,2,4]]
    let listOfSets = map Set.fromList [[1,2,3],[2,3,4],[1,2]]
    let solution   = map Set.fromList [[2,3,4],[1,2]]
    it "removeSupersets of complex example" $
      Set.fromList (removeSupersets listOfSets) `shouldBe` Set.fromList solution
