{-# LANGUAGE FlexibleContexts #-} -- This is enabled because of issues with the State monad below
module Plans.Execute (
  executePlan,
  wrapSpiral,
) where

-- TODO: Should we use the strict or the lazy state monad? Need to read up!
import Control.Exception.Base (assert)
import Control.Monad.State.Lazy
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

import Data.WordMaps.Algebraic as WordMap hiding (combine)
import qualified Data.TurningVector as TV
import Graphs.LabeledGraphInterface
import Lifting.CombinationGraph
import Plans.Spoke
import Plans.Plan
import Plans.CoveringGraph

{-
 We have a WordTree that maps addresses of Covering nodes to a pointed set. The
point denotes the center of the spiral at the turningWord of the Covering node
and the remaining elements in the set are the required nodes for the covering.
-}

executePlan :: Ord x => LabeledGraphInterface g x -> g -> Plan x -> (CombinationGraph x, Int)
executePlan gi g plan = (lg,dsl) where
  liftedGraph = Lifting.CombinationGraph.fromLabeledGraph gi g
  ((dsl,_),lg) = runState (runStateT lastNode WordMap.empty) liftedGraph
  lastNode = do zi <- constructNode plan zero
                oz <- constructNode plan one
                doubleSelfLoop <- lift $ combine zi oz
                return doubleSelfLoop

constructNode :: Ord x => Plan x -> CoveringNode
                          -> StateT (WordMap Int) (State (CombinationGraph x)) Int
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
                 return ()) -- do is needed for the typechecker that expects return value of type ()
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

wrapSpiral :: V.Vector (Spoke x) -> State (CombinationGraph x) (V.Vector Int)
wrapSpiral spokes = do
  lg <- get
  let emb = embed lg
      maximalDistanceInSpiral = maximum $ V.map maximalDistance spokes
      helper distance constructed =
        if distance > maximalDistanceInSpiral then return constructed
        else do let constructInSpoke plan soFar = 
                        foldM combine soFar
                              (map emb (pointsAtDistanceList distance plan))
                improved <- V.zipWithM constructInSpoke spokes constructed
                helper (distance + 1) improved
    in helper 1 (V.map (emb . hub) spokes)

wrapTentacle :: V.Vector CoveringNode -> V.Vector Int -> [(CoveringNode, Int)]
                -> State (CombinationGraph x) (V.Vector Int) 
wrapTentacle cycle fatVector tentacle = let
    startIndex = fromMaybe (error "tentacle does not seem to match the cycle")
                           (V.findIndex (== parent (fst . head $ tentacle)) cycle)
    turningVector = TV.fromVectorWithIndex startIndex fatVector
    operation fatNode (cn,cni) = combine fatNode cni
  in do tv <- TV.zipReverseWithListM operation turningVector tentacle
        return (TV.vector tv)
