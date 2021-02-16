module ArcCons (
  arcConsHomos,
) where

import Data.Map.Strict as Map
import Data.Set as Set
import Data.List
import Data.Maybe (maybeToList)

import Function
import Graph

type Approx x y = Map x (Set y)

approxToFunction :: (Ord x, Ord y) => Approx x y -> Either (Function x y) [Approx x y]
approxToFunction apx = worker (Map.toList (Map.map Set.toList apx)) Map.empty where
  worker [] accum = Left accum
  worker ((k,[]):_) _ = error "there is an issue"
  worker ((k,[s]):rest) accum = worker rest (Map.insert k s accum)
  worker ((k,list):rest) accum = Right (Prelude.map singelize list) where
    singelize s = Map.insert k (Set.singleton s) (Map.map Set.fromList (Map.fromList rest)) `Map.union` Map.map Set.singleton accum 

initialApprox :: Set x -> Set y -> Approx x y
initialApprox dom codom = Map.fromSet (\_ -> codom) dom

arcCons :: (Ord x, Ord y) => Graph x -> Graph y -> Approx x y
                                     -> Maybe (Approx x y)
arcCons d c approx = arcConsWorker initialWorklist approx where
  initialWorklist = concatMap (\a -> [(a,True),(a,False)]) (arcs d)
  arcConsWorker [] apx = Just apx
  arcConsWorker (((a,l,b),checkForward):list) apx = let
    guys = (if checkForward then successors else predecessors) c l
    (next,other) = if checkForward then (a,b) else (b,a)
    (remove,keep) = Set.partition (\candi -> guys candi `disjoint` (apx ! other)) (apx ! next)
    newApx = Map.insert next keep apx
    toAdd = if Set.null remove
      then []
      else concatMap (\l -> [((next,l,o),False) | o <- Set.toList $ successors d l next] ++
                            [((o,l,next),True) | o <- Set.toList $ predecessors d l next]) labels
      in if Set.null keep
           then Nothing
           else arcConsWorker (toAdd `Data.List.union` list) newApx

arcConsHomos :: (Ord x, Ord y) => Graph x -> Graph y -> [Function x y]
arcConsHomos d c = worker (initialApprox (Graph.domain d) (Graph.domain c)) where
  worker apx = do
    clean <- maybeToList (arcCons d c apx)
    case approxToFunction clean of
      Left fct         -> return fct
      Right splittings -> do
        split <- splittings
        worker split
