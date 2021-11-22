module MapGraph (
  MapGraph,
  mapGraphI, mapGraphIwithNodePrinter,
  fromGraph,
  subgraph,
  projection,
) where

import Control.Exception.Base
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Set.Extra as SetExtra

import qualified Graph

newtype MapGraph x = MapGraph { succPredMap :: Map x (Set x, Set x) }

mapGraphI :: (Ord x,Show x) => Graph.GraphI (MapGraph x) x
mapGraphI = mapGraphIwithNodePrinter show

mapGraphINoShow :: Ord x => Graph.GraphI (MapGraph x) x
mapGraphINoShow = mapGraphIwithNodePrinter undefined

mapGraphIwithNodePrinter :: Ord x => (x -> String) -> Graph.GraphI (MapGraph x) x
mapGraphIwithNodePrinter prettyNode = Graph.interfaceFromSuccPredPretty
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

fromGraph :: Ord a => Graph.GraphI g a -> g -> MapGraph a
fromGraph gi graph = 
  assert (Graph.succPredInDom mapGraphINoShow result) $
  assert (Graph.succPredMatch mapGraphINoShow result) result where
    result = MapGraph spm
    dom = Graph.domain gi graph
    spmapping v = (Graph.successors gi graph v, Graph.predecessors gi graph v)
    spm = Map.fromSet spmapping dom

subgraph :: Ord a => Graph.GraphI g a -> g -> Set a -> MapGraph a
subgraph gi g subdomain = assert (subdomain `isSubsetOf` Graph.domain gi g) $
  assert (Graph.succPredInDom mapGraphINoShow result) $
  assert (Graph.succPredMatch mapGraphINoShow result) result where
    result = MapGraph spm
    dom = subdomain
    spmapping v = (Graph.successors gi g v `Set.intersection` subdomain,
                   Graph.predecessors gi g v `Set.intersection` subdomain)
    spm = Map.fromSet spmapping dom

projection :: (Ord a, Ord b) => Graph.GraphI g a -> g -> (a -> b) -> MapGraph b
projection gi g projection =
  assert (Graph.succPredInDom mapGraphINoShow result) $
  assert (Graph.succPredMatch mapGraphINoShow result) result where
    result = MapGraph spm
    oldDomain = Graph.domain gi g
    dom = Set.map projection oldDomain
    preimage n = Set.filter (\m -> projection m == n) oldDomain
    mapper direction = SetExtra.concatMap (Set.map projection . direction gi g) . preimage
    spmapping v = (mapper Graph.successors v, mapper Graph.predecessors v)
    spm = Map.fromSet spmapping dom

instance (Ord x, Show x) => Show (MapGraph x) where
  show = unlines . (Graph.prettyGraph mapGraphI)

