module CaleyGraph (
  CaleyGraph,
  relationOfWord,
  rightCaleyGraph,
  isGood,
  isPossibleValue,  
) where

import Control.Exception.Base
import Data.List.Ordered
import Data.Map.Strict as Map
import Data.Set as Set

import Label
import UnlabeledBitGraph

data CaleyGraph = CaleyGraph {
  successorMap :: Map.Map UnlabeledBitGraph (UnlabeledBitGraph,UnlabeledBitGraph),
  -- nonWellfoundedElements is the subset of the domain that is hit by infinitely many words over {Zero, One}.
  wellfoundedElements :: Set.Set UnlabeledBitGraph,
  nonWellfoundedElements :: Set.Set UnlabeledBitGraph
} deriving Show

domain :: CaleyGraph -> Set UnlabeledBitGraph
domain = keysSet . successorMap

successor :: CaleyGraph -> UnlabeledBitGraph -> Label -> UnlabeledBitGraph
successor cg node label = assert (node `elem` domain cg) $
  case label of
    Zero -> fst $ (successorMap cg ! node)
    One  -> snd $ (successorMap cg ! node)

rightCaleyGraph :: Size -> (UnlabeledBitGraph,UnlabeledBitGraph) -> CaleyGraph
rightCaleyGraph size (zeroRel,oneRel) =
  CaleyGraph succMap wellfounded nonWellfounded where
    keepAdding [] m p = (m,p)
    keepAdding (next:rest) m p = let
      zeroSucc = compose size next zeroRel
      oneSucc = compose size next oneRel
      newM = Map.insert next (zeroSucc,oneSucc) m
        -- I think the flip in union is potentialy more efficient because Set.union should be used for bigset `union` smallset but insertWith calls the provided function f with f new_value old_value. (This propbabely does not matter for now!)
      newPred' = Map.insertWith (flip Set.union) zeroSucc (Set.singleton next) p
      newPred = Map.insertWith (flip Set.union) oneSucc (Set.singleton next) newPred'
      adder elem = if elem `Map.member` newM then id else insertSet elem
      newWorklist = adder zeroSucc $ adder oneSucc rest
        in keepAdding newWorklist newM newPred
    (succMap, predMap) = keepAdding [diagonal size] Map.empty Map.empty
    computeWellfounded [] finites = finites
    computeWellfounded (next:rest) finites = let
      in if Map.findWithDefault Set.empty next predMap `Set.isSubsetOf` finites
           then let (zeroSucc,oneSucc) = succMap ! next
                    adder elem = if elem `Set.notMember` finites
                                    then insertSet elem
                                    else id
                    restPlus = adder zeroSucc $ adder oneSucc rest
             in computeWellfounded restPlus (Set.insert next finites)
           else computeWellfounded rest finites
    wellfounded = computeWellfounded [diagonal size] Set.empty
    nonWellfounded = (keysSet succMap) Set.\\ wellfounded

relationOfWord :: Size -> CaleyGraph -> [Label] -> UnlabeledBitGraph
relationOfWord size cg word = Prelude.foldl (successor cg) (diagonal size) word

isGood :: Size -> CaleyGraph -> Bool
isGood size cg = all (not . hasNoRefl size) (wellfoundedElements cg) &&
                 all (hasUniv size) (nonWellfoundedElements cg)

isPossibleValue :: Size -> CaleyGraph -> [Label] -> [Node] -> Node -> Bool
isPossibleValue size cg word others node =
  all (\v -> hasArc size (relationOfWord size cg word) (node,v)) others

