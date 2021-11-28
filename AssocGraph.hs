module AssocGraph (
  AssocGraph(AssocGraph),
  assocGraphI, assocGraphINotPretty, assocGraphIwithNodePrinter,
  fromGraph,
) where

import Control.Exception.Base
import qualified Data.Set as Set
import qualified Data.Set.Extra

import qualified Graph
import Pretty

{- This representation does not explicitely store the domain. Thus it is not able to correctely represent graphs which contain nodes that are not part of any arc! -}
newtype AssocGraph a = AssocGraph {arcs :: [(a,a)]}

assocGraphI :: (Ord a, Pretty a) => Graph.GraphI (AssocGraph a) a
assocGraphI = assocGraphIwithNodePrinter pretty

assocGraphINotPretty :: Ord x => Graph.GraphI (AssocGraph x) x
assocGraphINotPretty = assocGraphIwithNodePrinter (error "can not show nodes of this graph")

assocGraphIwithNodePrinter :: Ord x => (x -> String) -> Graph.GraphI (AssocGraph x) x
assocGraphIwithNodePrinter prettyNode = Graph.interfaceFromArcsPretty
                                         arcs
                                         (\_ n -> prettyNode n) 

isNubList :: Eq a => [a] -> Bool
isNubList list = worker [] list where
  worker _ [] = True
  worker seen (next:rest) = not (next `elem` seen) && (worker (next:seen) rest)

fromGraph :: Ord a => Graph.GraphI g a -> g -> AssocGraph a
fromGraph gi g = assert (isNubList list) $ AssocGraph list where
  list = Graph.arcs gi g

{-
fromGraph :: Ord a => Graph.GraphI g a -> g -> AssocGraph a
fromGraph gi g = materialize $ graphAsAssocGraph gi g

isNubby :: Eq a => AssocGraph a -> Bool
isNubby ag = isNubList (ag Graph.Zero) && isNubList (ag Graph.One)

materialize :: Eq a => AssocGraph a -> AssocGraph a
materialize ag = fromPair (ag Graph.Zero, ag Graph.One)

fromPair :: Eq a => ([(a,a)],[(a,a)]) -> AssocGraph a
fromPair (zList,oList) = assert (isNubby newGraph) $ newGraph where
  newGraph Graph.Zero = zList
  newGraph Graph.One = oList


applyBijection :: Eq b => (a -> b) -> AssocGraph a -> AssocGraph b
applyBijection b assocGraph = materialize $ mapper . assocGraph where
  liftedb (u,v) = (b u, b v)
  mapper = map liftedb

subgraph :: Ord a => Set.Set a -> AssocGraph a -> AssocGraph a
subgraph set aGraph = materialize $ (\l -> filter pred (aGraph l)) where
  pred (x,y) = x `Set.member` set && y `Set.member` set

assocToMap :: Ord a => AssocGraph a -> MapGraph.MapGraph a
assocToMap = MapGraph.fromGraph assocGraphI

addNodesWithSuccs :: Ord a => [(a,(Set.Set a, Set.Set a))] -> AssocGraph a -> AssocGraph a
addNodesWithSuccs toAdd graph = fromPair $
  (foldl (\list (n,(sz,_)) -> [(n,v) | v <- Set.toList sz] ++ list) (graph Graph.Zero) toAdd,
   foldl (\list (n,(_,so)) -> [(n,v) | v <- Set.toList so] ++ list) (graph Graph.One) toAdd)

addNodesWithPreds :: Ord a => [(a,(Set.Set a, Set.Set a))] -> AssocGraph a -> AssocGraph a
addNodesWithPreds toAdd graph = fromPair $
  (foldl (\list (n,(sz,_)) -> [(v,n) | v <- Set.toList sz] ++ list) (graph Graph.Zero) toAdd,
   foldl (\list (n,(_,so)) -> [(v,n) | v <- Set.toList so] ++ list) (graph Graph.One) toAdd)
-}

instance (Ord x, Pretty x) => Show (AssocGraph x) where
  show = Graph.showG assocGraphI
