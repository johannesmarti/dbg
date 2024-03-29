module Graphs.AssocGraph (
  AssocGraph(AssocGraph),
  assocGraphInterface, assocGraphInterfaceNotPretty, assocGraphInterfacewithNodePrinter,
  fromGraph,
) where

import Control.Exception.Base
import qualified Data.Set as Set

import Graphs.GraphInterface as GI
import Graphs.PrettyNode

{- This representation does not explicitely store the domain. Thus it is not able to correctely represent graphs which contain nodes that are not part of any arc! -}
newtype AssocGraph a = AssocGraph {arcs :: [(a,a)]}

assocGraphInterface :: (Ord a, PrettyNode a) => GI.GraphInterface (AssocGraph a) a
assocGraphInterface = assocGraphInterfacewithNodePrinter pretty

assocGraphInterfaceNotPretty :: Ord x => GI.GraphInterface (AssocGraph x) x
assocGraphInterfaceNotPretty = assocGraphInterfacewithNodePrinter (error "can not show nodes of this graph")

assocGraphInterfacewithNodePrinter :: Ord x => (x -> String) -> GI.GraphInterface (AssocGraph x) x
assocGraphInterfacewithNodePrinter prettyNode = GI.interfaceFromArcsPretty
                                         Graphs.AssocGraph.arcs
                                         (\_ n -> prettyNode n) 

isNubList :: Eq a => [a] -> Bool
isNubList list = worker [] list where
  worker _ [] = True
  worker seen (next:rest) = not (next `elem` seen) && (worker (next:seen) rest)

fromGraph :: Ord a => GI.GraphInterface g a -> g -> AssocGraph a
fromGraph gi g = assert (isNubList list) $ AssocGraph list where
  list = GI.arcs gi g

{-
fromGraph :: Ord a => GI.GraphInterface g a -> g -> AssocGraph a
fromGraph gi g = materialize $ graphAsAssocGraph gi g

isNubby :: Eq a => AssocGraph a -> Bool
isNubby ag = isNubList (ag GI.Zero) && isNubList (ag GI.One)

materialize :: Eq a => AssocGraph a -> AssocGraph a
materialize ag = fromPair (ag GI.Zero, ag GI.One)

fromPair :: Eq a => ([(a,a)],[(a,a)]) -> AssocGraph a
fromPair (zList,oList) = assert (isNubby newGraph) $ newGraph where
  newGraph GI.Zero = zList
  newGraph GI.One = oList


applyBijection :: Eq b => (a -> b) -> AssocGraph a -> AssocGraph b
applyBijection b assocGraph = materialize $ mapper . assocGraph where
  liftedb (u,v) = (b u, b v)
  mapper = map liftedb

subgraph :: Ord a => Set.Set a -> AssocGraph a -> AssocGraph a
subgraph set aGraph = materialize $ (\l -> filter pred (aGraph l)) where
  pred (x,y) = x `Set.member` set && y `Set.member` set

assocToMap :: Ord a => AssocGraph a -> MapGraphInterface.MapGraph a
assocToMap = MapGraphInterface.fromGraph assocGraphInterface

addNodesWithSuccs :: Ord a => [(a,(Set.Set a, Set.Set a))] -> AssocGraph a -> AssocGraph a
addNodesWithSuccs toAdd graph = fromPair $
  (foldl (\list (n,(sz,_)) -> [(n,v) | v <- Set.toList sz] ++ list) (graph GraphInterface.Zero) toAdd,
   foldl (\list (n,(_,so)) -> [(n,v) | v <- Set.toList so] ++ list) (graph GraphInterface.One) toAdd)

addNodesWithPreds :: Ord a => [(a,(Set.Set a, Set.Set a))] -> AssocGraph a -> AssocGraph a
addNodesWithPreds toAdd graph = fromPair $
  (foldl (\list (n,(sz,_)) -> [(v,n) | v <- Set.toList sz] ++ list) (graph GraphInterface.Zero) toAdd,
   foldl (\list (n,(_,so)) -> [(v,n) | v <- Set.toList so] ++ list) (graph GraphInterface.One) toAdd)
-}

instance (Ord x, PrettyNode x) => Show (AssocGraph x) where
  show = GI.showG assocGraphInterface
