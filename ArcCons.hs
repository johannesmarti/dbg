module ArcCons (
  Approx,
  hasSplit,
  arcConsHomos,
) where

import Data.Map.Strict as Map
import Data.Set as Set
import Data.List
import Data.Maybe (maybeToList)

import Function
import Graph
import Homo

type Approx x y = Map x (Set y)

initialApprox :: Set x -> Set y -> Approx x y
initialApprox dom codom = Map.fromSet (\_ -> codom) dom

{- Turns an approximation into a function. It assumes that the approximation is
never empty at any value. -}
readOffFunction :: Approx x y -> Function x y
readOffFunction = Map.map (Set.elemAt 0)

type Worklist x = [(Arc x,Bool)]

completeWorklist :: Ord x => Graph x -> Worklist x
completeWorklist graph = concatMap (\a -> [(a,True),(a,False)]) (arcs graph)

addNodeToWorklist :: Ord x => Graph x -> x -> Worklist x -> Worklist x
addNodeToWorklist graph node worklist = newItems `Data.List.union` worklist
  where
    newItems = concatMap itemsForLabel labels
    itemsForLabel l =
      [((node,l,o),False) | o <- Set.toList $ successors graph l node]
        ++ [((o,l,node),True) | o <- Set.toList $ predecessors graph l node]


arcCons :: (Ord x, Ord y) => Graph x -> Graph y -> Approx x y
                                     -> Maybe (Approx x y)
arcCons d c approx = arcConsInner d c (completeWorklist d) approx

arcConsInner :: (Ord x, Ord y) => Graph x -> Graph y -> Worklist x
                                  -> Approx x y -> Maybe (Approx x y)
arcConsInner d c worklist approx = arcConsWorker worklist approx where
  arcConsWorker [] apx = Just apx
  arcConsWorker (((a,l,b),checkForward):list) apx = let
    guys = (if checkForward then successors else predecessors) c l
    (next,other) = if checkForward then (a,b) else (b,a)
    (remove,keep) = Set.partition (\candi -> guys candi `disjoint` (apx ! other)) (apx ! next)
    newApx = Map.insert next keep apx
    newWorklist = if Set.null remove
                    then list
                    else addNodeToWorklist d next list
      in if Set.null keep
           then Nothing
           else arcConsWorker newWorklist newApx

hasSplit :: (Ord x,Ord y) => Approx x y -> Maybe x
hasSplit approx = fmap fst (pickElemWith (not . isSingleton) approx) where
  isSingleton set = Set.size set <= 1
  pickElemWith predicate = Map.lookupMax . (Map.filter predicate)

splittingsAt :: (Ord x, Ord y) => Approx x y -> x -> [Approx x y]
splittingsAt approx node = let
    valueList = Set.toList (approx ! node)
    singlified s = Map.insert node (Set.singleton s) approx
  in Prelude.map singlified valueList

arcConsHomos :: (Ord x, Ord y) => HomoSearch x y
arcConsHomos d c = let
    worker worklist apx = do
      clean <- maybeToList (arcConsInner d c worklist apx)
      case hasSplit clean of
        Nothing        -> return (readOffFunction clean)
        Just splitNode -> do
                    split <- splittingsAt clean splitNode
                    worker (addNodeToWorklist d splitNode []) split
  in worker (completeWorklist d) (initialApprox (Graph.domain d) (Graph.domain c))
