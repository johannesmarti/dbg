module Graphs.MapGraph (
  MapGraph,
  mapGraphInterface, mapGraphInterfaceNotPretty, mapGraphInterfaceWithNodePrinter,
  fromGraph,
  subgraph,
  projection,
  applyBijection,
  addNode,
  addArc,
  addNodes,
  addArcs,
) where

import Control.Exception.Base
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Set.Extra as SetExtra

import qualified Graphs.GraphInterface as GI
import Graphs.PrettyNode

newtype MapGraph x = MapGraph { succPredMap :: Map x (Set x, Set x) }

mapGraphInterface :: (Ord x, PrettyNode x) => GI.GraphInterface (MapGraph x) x
mapGraphInterface = mapGraphInterfaceWithNodePrinter pretty

mapGraphInterfaceNotPretty :: Ord x => GI.GraphInterface (MapGraph x) x
mapGraphInterfaceNotPretty = mapGraphInterfaceWithNodePrinter (error "can not show nodes of this graph")

mapGraphInterfaceWithNodePrinter :: Ord x => (x -> String) -> GI.GraphInterface (MapGraph x) x
mapGraphInterfaceWithNodePrinter prettyNode = GI.interfaceFromSuccPredPretty
                                         domain successors predecessors
                                         (\_ n -> prettyNode n) 

domain :: Ord x => MapGraph x -> Set x
domain = Map.keysSet . succPredMap

succPredPair :: Ord x => MapGraph x -> x -> (Set x, Set x)
succPredPair mg v = assert (v `Set.member` domain mg) $
  findWithDefault (error "node not in mapGraph") v (succPredMap mg)

successors :: Ord x => MapGraph x -> x -> Set x
successors mg v = fst $ succPredPair mg v

predecessors :: Ord x => MapGraph x -> x -> Set x
predecessors mg v = snd $ succPredPair mg v

fromGraph :: Ord a => GI.GraphInterface g a -> g -> MapGraph a
fromGraph gi graph = 
  assert (GI.succPredInDom mapGraphInterfaceNotPretty result) $
  assert (GI.succPredMatch mapGraphInterfaceNotPretty result) result where
    result = MapGraph spm
    dom = GI.domain gi graph
    spmapping v = (GI.successors gi graph v, GI.predecessors gi graph v)
    spm = Map.fromSet spmapping dom

subgraph :: Ord a => GI.GraphInterface g a -> g -> Set a -> MapGraph a
subgraph gi g subdomain = assert (subdomain `isSubsetOf` GI.domain gi g) $
  assert (GI.succPredInDom mapGraphInterfaceNotPretty result) $
  assert (GI.succPredMatch mapGraphInterfaceNotPretty result) result where
    result = MapGraph spm
    dom = subdomain
    spmapping v = (GI.successors gi g v `Set.intersection` subdomain,
                   GI.predecessors gi g v `Set.intersection` subdomain)
    spm = Map.fromSet spmapping dom

projection :: (Ord a, Ord b) => GI.GraphInterface g a -> g -> (a -> b) -> MapGraph b
projection gi g projection =
  assert (GI.succPredInDom mapGraphInterfaceNotPretty result) $
  assert (GI.succPredMatch mapGraphInterfaceNotPretty result) result where
    result = MapGraph spm
    oldDomain = GI.domain gi g
    dom = Set.map projection oldDomain
    preimage n = Set.filter (\m -> projection m == n) oldDomain
    mapper direction = SetExtra.concatMap (Set.map projection . direction gi g) . preimage
    spmapping v = (mapper GI.successors v, mapper GI.predecessors v)
    spm = Map.fromSet spmapping dom

applyBijection :: (Ord a, Ord b) => GI.GraphInterface g a -> g -> (a -> b) -> MapGraph b
applyBijection gi g b =
  assert (GI.succPredInDom mapGraphInterfaceNotPretty result) $
  assert (GI.succPredMatch mapGraphInterfaceNotPretty result) result where
    result = MapGraph nmg
    oldDomain = GI.domain gi g
    mapper direction = Set.map b . direction gi g
    spmapping v = (mapper GI.successors v, mapper GI.predecessors v)
    spm = Map.fromSet spmapping oldDomain
    nmg = Map.mapKeys b spm

addNode :: Ord x => MapGraph x -> x -> MapGraph x
addNode (MapGraph map) node = assert (node `Map.notMember` map) $
  MapGraph $ Map.insert node (Set.empty,Set.empty) map

addNodes :: Ord x => MapGraph x -> [x] -> MapGraph x
addNodes g list = Prelude.foldl addNode g list

addArc :: Ord x => MapGraph x -> (x,x) -> MapGraph x
addArc (MapGraph map) (v,w) = assert (v `Map.member` map) $
                              assert (w `Map.member` map) $
                              assert (GI.hasArc mapGraphInterfaceNotPretty res (v,w)) res where
  insw (ss,ps) = (Set.insert w ss, ps)
  insv (ss,ps) = (ss, Set.insert v ps)
  imap = Map.adjust insw v map
  jmap = Map.adjust insv w imap
  res = MapGraph jmap

addArcs :: Ord x => MapGraph x -> [(x,x)] -> MapGraph x
addArcs g list = Prelude.foldl addArc g list

instance (Ord x, PrettyNode x) => Show (MapGraph x) where
  show = GI.showG mapGraphInterface
