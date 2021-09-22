module ArcCons (
  Approx,
  hasSplit,
  arcConsHomosFromApprox,
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

fullApprox :: Set x -> Set y -> Approx x y
fullApprox dom codom = Map.fromSet (\_ -> codom) dom

{- Turns an approximation into a function. It assumes that the approximation is
never empty at any value. -}
readOffFunction :: Approx x y -> Function x y
readOffFunction = Map.map (Set.elemAt 0)

type Worklist x = [(Arc x,Bool)]

completeWorklist :: Ord x => GraphI g x -> g -> Worklist x
completeWorklist gi graph = concatMap (\a -> [(a,True),(a,False)]) (arcs gi graph)

addNodeToWorklist :: Ord x => GraphI g x -> g -> x -> Worklist x -> Worklist x
addNodeToWorklist gi graph node worklist = newItems `Data.List.union` worklist
  where
    newItems = concatMap itemsForLabel labels
    itemsForLabel l =
      [((node,l,o),False) | o <- Set.toList $ successors gi graph l node]
        ++ [((o,l,node),True) | o <- Set.toList $ predecessors gi graph l node]


arcCons :: (Ord x, Ord y) => GraphI g1 x -> GraphI g2 y -> g1 -> g2
                                     -> Approx x y -> Maybe (Approx x y)
arcCons di ci d c approx = arcConsInner di ci d c (completeWorklist di d) approx

arcConsInner :: (Ord x, Ord y) => GraphI g1 x -> GraphI g2 y -> g1 -> g2 -> Worklist x
                                  -> Approx x y -> Maybe (Approx x y)
arcConsInner di ci d c worklist approx = arcConsWorker worklist approx where
  arcConsWorker [] apx = Just apx
  arcConsWorker (((a,l,b),checkForward):list) apx = let
    guys = (if checkForward then successors else predecessors) ci c l
    (next,other) = if checkForward then (a,b) else (b,a)
    (remove,keep) = Set.partition (\candi -> guys candi `Set.disjoint` (apx ! other)) (apx ! next)
    newApx = Map.insert next keep apx
    newWorklist = if Set.null remove
                    then list
                    else addNodeToWorklist di d next list
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


arcConsHomosFromApprox :: (Ord x, Ord y) => GraphI g1 x -> GraphI g2 y
                            -> Approx x y -> HomoSearch g1 g2 x y
arcConsHomosFromApprox di ci approx d c = let
    worker worklist apx = do
      clean <- maybeToList (arcConsInner di ci d c worklist apx)
      case hasSplit clean of
        Nothing        -> return (readOffFunction clean)
        Just splitNode -> do
                    split <- splittingsAt clean splitNode
                    worker (addNodeToWorklist di d splitNode []) split
  in worker (completeWorklist di d) approx

arcConsHomos :: (Ord x, Ord y) => GraphI g1 x -> GraphI g2 y -> HomoSearch g1 g2 x y
arcConsHomos di ci d c = arcConsHomosFromApprox di ci
  (fullApprox (Graph.domain di d) (Graph.domain ci c)) d c

