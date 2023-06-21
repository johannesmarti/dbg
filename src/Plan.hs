{-# LANGUAGE FlexibleContexts #-} -- This is enabled because of issues with the State monad below
module Plan (
  Spoke,
  spoke,
  Plan,
  Plan.empty,
  Plan.insert,
) where

-- TODO: Should we use the strict or the lazy state monad? Need to read up!
import Control.Monad.State.Lazy
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

import CoveringGraph
import LabeledGraph
import LiftedGraph

import WordMap.Algebraic as WordMap

{-
 We have a WordTree that maps addresses of Covering nodes to a pointed set. The
point denotes the center of the spiral at the turningWord of the Covering node
and the remaining elements in the set are the required nodes for the covering.
-}

-- TODO: This data type should be used or unified with the implementation of Spiral
data Spoke x = Spoke {
  hub    :: x,
  points :: M.Map x Int
}

pointsAtDistance :: Int -> Spoke x -> [x]
pointsAtDistance distance =
  map fst . filter (\(p,d) -> d == distance) . M.toList . points

maxDistance :: Spoke x -> Int
maxDistance = maximum . M.elems . points

spoke :: Ord x => x -> [(x,Int)] -> Spoke x
spoke h p = Spoke h (M.insert h 0 (M.fromList p))

type Plan x = WordMap (Spoke x)

empty :: Plan x
empty = WordMap.empty

insert :: [Label] -> Spoke x -> Plan x -> Plan x
insert = WordMap.insert

executePlan :: Ord x => LabeledGraphI g x -> g -> Plan x -> LiftedGraph x
executePlan gi g plan = result where
  result = undefined

{-
wrap-up/roll a cycle:
- Wrap up the nodes on the spiral for the cycle
-- Count up d = 1 .. n and wrap up all nodes with distance d (Once nodes for d hhave been wrapped we should be able to wrap nodes of dinstance d + 1) At each stage keep track of the node at the hub.
-- Check that all the children of the nodes on the hub have been constructed and return them in a reasonable order.
-- wrap these children in a raesonable order.
-- store the fat hub nodes in the partial result word map

Things to be done:
- List nodes in plan for a spiral
-}

{-
data PartialResult x = PartialResult {
  liftedGraph :: LiftedGraph x,
  constructedNodes :: WordMap Int
}
-}

constructNode :: Plan x -> CoveringNode
                 -> StateT (WordMap Int) (State (LiftedGraph x)) Int
constructNode plan coveringNode = let
    lookupNodeWrapper otherAction = do
      constructed <- get
      case WordMap.lookup (address coveringNode) constructed of
        Just n  -> return n
        Nothing -> otherAction
    myCycleList = cycleOfNode coveringNode
    ancestors = map properlyAscendingPredecessor myCycleList
    myCycle = V.fromList myCycleList
    planVector = V.map (maybe (error "node should be in map of plans") id . (\a -> WordMap.lookup a plan) . address) myCycle
  in lookupNodeWrapper $ do
    -- Make sure that the ancestor of all nodes on the cycle have been completely wrapped.
    mapM_ (constructNode plan) ancestors
    fatVector <- lift $ wrapSpiral planVector
    -- need to wrap up further over the fat vector

    -- TODO: The following two lines could be improved
    constructed <- get
    put $ V.ifoldl (\m i node -> WordMap.insert (address (myCycle V.! i)) node m)
                   constructed fatVector
    return 32

wrapSpiral :: (V.Vector (Spoke x)) -> State (LiftedGraph x) (V.Vector Int)
wrapSpiral spokes = undefined

--wrapSpiral :: Vec.Vector (Spoke x) -> State (LiftedGraph x) (Vec.Vector Int)
--wrapSpiral _ = undefined
