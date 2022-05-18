module ConstructionGraph (
  constructionGraphI,
  visibleInConstructionGraph,
  immediatelyConstructible,
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Label
import LabeledGraph
import Tools (strictPairs)

constructionGraphI :: Ord x => LabeledGraphI g x -> LabeledGraphI g (x,x)
constructionGraphI gi = let
    dom = Set.fromList . strictPairs . Set.toList . (domain gi)
    hasA g label ((a,b),(x,y)) = let
        ha = hasArc gi g label
      in (ha (a,x) || ha (b,x)) && (ha (a,y) || ha (b,y))
    pretN g (a,b) = "[" ++ (prettyNode gi g a) ++ " " ++ (prettyNode gi g b) ++ "]"
  in interfaceFromHasArcPretty dom hasA pretN

visibleInConstructionGraph :: LabeledGraphI g x -> g -> Label -> (x,x) -> Bool
visibleInConstructionGraph gi g l (a,b) = any sees (domain gi g) where
  sees old = hasArc gi g l (old,a) && hasArc gi g l (old,b)

immediatelyConstructible :: Ord x => LabeledGraphI g x -> g -> Set.Set (x,x)
immediatelyConstructible gi g = Set.filter visible dom where
  dom = domain (constructionGraphI gi) g
  visible (a,b) = all (\l -> visibleInConstructionGraph gi g l (a,b)) labelsList
