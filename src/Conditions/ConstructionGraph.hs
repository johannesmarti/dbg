module Conditions.ConstructionGraph (
  constructionGraphInterface,
  visibleInConstructionGraph,
  immediatelyConstructible,
  powerGraphInterface,
  universalReachability,
  prettyReachability,
) where

import qualified Data.Set as Set
import qualified Data.Set.Extra as SE
import Data.List (intercalate, intersperse)

import Data.Label
import Graphs.LabeledGraphInterface
import Tools (strictPairs)

constructionGraphInterface :: Ord x => LabeledGraphInterface g x -> LabeledGraphInterface g (x,x)
constructionGraphInterface gi = let
    dom = Set.fromList . strictPairs . Set.toList . (domain gi)
    hasA g label ((a,b),(x,y)) = let
        ha = hasArc gi g label
      in (ha (a,x) || ha (b,x)) && (ha (a,y) || ha (b,y))
    pret g (a,b) = let
        pr = prettyNode gi g
      in "[" ++ pr a ++ " " ++ pr b ++ "]"
  in iFromHasArcPretty dom hasA pret

visibleInConstructionGraph :: LabeledGraphInterface g x -> g -> Label -> (x,x) -> Bool
visibleInConstructionGraph gi g l (a,b) = any sees (domain gi g) where
  sees old = hasArc gi g l (old,a) && hasArc gi g l (old,b)

immediatelyConstructible :: Ord x => LabeledGraphInterface g x -> g -> Set.Set (x,x)
immediatelyConstructible gi g = Set.filter visible dom where
  dom = domain (constructionGraphInterface gi) g
  visible (a,b) = all (\l -> visibleInConstructionGraph gi g l (a,b)) labelsList

powerGraphInterface :: Ord x => LabeledGraphInterface g x -> LabeledGraphInterface g (Set.Set x)
powerGraphInterface gi = let
    dom g = Set.delete Set.empty (Set.powerSet (domain gi g))
    {- This could be done much more efficiently by computng the biggest successor set and then taking all subsets. -}
    hasA g label (u,v) = all (\y -> any (\x -> hasArc gi g label (x,y)) u) v
    pret g u = "{" ++ intercalate ", " (map (prettyNode gi g) $ Set.toList u)
                   ++ "}"
  in iFromHasArcPretty dom hasA pret

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

prettyReachability :: (x -> String) -> [Set.Set x] -> [String]
prettyReachability nodePrinter reachabilitySpheres = let
    printSphere set = intercalate ", " (map nodePrinter $ Set.toList set)
  in intersperse "----------------------" $ map printSphere reachabilitySpheres
