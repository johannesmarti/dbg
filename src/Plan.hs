{-# LANGUAGE FlexibleContexts #-} -- This is enabled because of issues with the State monad below
module Plan (
  Spoke,
  spoke,
  Plan,
  Plan.empty,
  Plan.insert,
  executePlan,
  wrapSpiral,
) where

-- TODO: Should we use the strict or the lazy state monad? Need to read up!
import Control.Exception.Base (assert)
import Control.Monad.State.Lazy
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import CoveringGraph
import LabeledGraph
import LiftedGraph

import WordMap.Algebraic as WordMap
import qualified TurningVector as TV

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

maximalDistance :: Spoke x -> Int
maximalDistance = maximum . M.elems . points

spoke :: Ord x => x -> [(x,Int)] -> Spoke x
spoke h p = Spoke h (M.insert h 0 (M.fromList p))

isSingleton :: Spoke x -> Bool
isSingleton spoke = M.size (points spoke) <= 1

singletonNode :: Spoke x -> Maybe x
singletonNode spoke = if isSingleton spoke
                        then Just (hub spoke)
                        else Nothing

contained :: Ord x => x -> Spoke x -> Bool
contained x (Spoke _ map) = x `M.member` map

type Plan x = WordMap (Spoke x)

empty :: Plan x
empty = WordMap.empty

insert :: [Label] -> Spoke x -> Plan x -> Plan x
insert = WordMap.insert

executePlan :: Ord x => LabeledGraphI g x -> g -> Plan x -> (LiftedGraph x, Int)
executePlan gi g plan = (lg,dsl) where
  liftedGraph = LiftedGraph.fromLGraph gi g
  ((dsl,_),lg) = runState (runStateT lastNode WordMap.empty) liftedGraph
  lastNode = do zi <- constructNode plan zero
                oz <- constructNode plan one
                doubleSelfLoop <- lift $ LiftedGraph.combine zi oz
                return doubleSelfLoop

constructNode :: Ord x => Plan x -> CoveringNode
                          -> StateT (WordMap Int) (State (LiftedGraph x)) Int
constructNode plan coveringNode = let
    lookupNodeWrapper otherAction = do
      constructed <- get
      case WordMap.lookup (address coveringNode) constructed of
        Just n  -> return n
        Nothing -> otherAction
    myCycleList = cycleOfNode coveringNode
    myIndex = 0
    planOfNode cn = forceLookup (address cn) plan
    needsCounterPoint cn = case singletonNode $ planOfNode cn of
                             Just a  -> let par = parent cn
                                        in if par == epsilon
                                             then True
                                             else not (a `contained` planOfNode par)
                             Nothing -> True
    constructCounterPoint node =
        when (needsCounterPoint node)
             (do constructNode plan (properlyAscendingPredecessor node)
                 return ())
    myCycle = V.fromList myCycleList
    planVector = V.map ((\a -> WordMap.forceLookup a plan) . address) myCycle
    inDom cn = inDomain (address cn) plan
    liftingsOfCycle = childCycles inDom coveringNode
    fatTentacles = map reverse liftingsOfCycle
    pairNodes cn = do intNode <- constructNode plan cn
                      return (cn, intNode)
  in lookupNodeWrapper $ do
    -- Make sure that the counterpoints of all nodes on the cycle have been completely wrapped up.
    mapM_ constructCounterPoint myCycleList
    fatVector <- lift $ wrapSpiral planVector
    -- it might be that it is not needed to construct the nodes in the tentacles because they have already been constructed. I am not sure about this!

    -- wrap up the childCycles
    fatterTentacles <- mapM (mapM pairNodes) fatTentacles
    fatterVector <- lift $ foldM (wrapTentacle myCycle) fatVector fatterTentacles

    -- TODO: The following four lines could be improved
    constructed <- get
    put $ V.ifoldl (\m i node -> WordMap.insert (address (myCycle V.! i)) node m)
                   constructed fatterVector
    return (assert ((myCycle V.! myIndex) == coveringNode) 
                   (fatterVector V.! myIndex))

wrapSpiral :: (V.Vector (Spoke x)) -> State (LiftedGraph x) (V.Vector Int)
wrapSpiral spokes = do
  lg <- get
  let emb = embed lg
      maximalDistanceInSpiral = maximum $ V.map maximalDistance spokes
      helper distance constructed =
        if distance > maximalDistanceInSpiral then return constructed
        else do let constructInSpoke plan soFar = 
                        foldM LiftedGraph.combine soFar
                              (map emb (pointsAtDistance distance plan))
                improved <- V.zipWithM constructInSpoke spokes constructed
                helper (distance + 1) improved
    in helper 1 (V.map (emb . hub) spokes)

wrapTentacle :: V.Vector CoveringNode -> V.Vector Int -> [(CoveringNode, Int)]
                -> State (LiftedGraph x) (V.Vector Int) 
wrapTentacle cycle fatVector tentacle = let
    startIndex = fromMaybe (error "tentacle does not seem to match the cycle")
                           (V.findIndex (== parent (fst . head $ tentacle)) cycle)
    turningVector = TV.fromVectorWithIndex startIndex fatVector
    operation fatNode (cn,cni) = LiftedGraph.combine fatNode cni
  in do tv <- TV.zipReverseWithListM operation turningVector tentacle
        return (TV.vector tv)
