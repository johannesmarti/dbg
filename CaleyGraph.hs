module CaleyGraph (
  CaleyGraph,
  relationOfWord,
  rightCaleyGraph,
  isGood,
  isGoodForDom,
  isPossibleValue,  
) where

import Control.Exception.Base
import Data.List.Ordered
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Label
import UnlabeledBitGraph

data CaleyGraph = CaleyGraph {
  successorMap :: Map.Map UnlabeledBitGraph (UnlabeledBitGraph,UnlabeledBitGraph),
  -- nonWellfoundedElements is the subset of the domain that is hit by infinitely many words over {Zero, One}.
  wellfoundedElements :: Set.Set UnlabeledBitGraph,
  nonWellfoundedElements :: Set.Set UnlabeledBitGraph
} deriving Show

domain :: CaleyGraph -> Set.Set UnlabeledBitGraph
domain = Map.keysSet . successorMap

successor :: CaleyGraph -> UnlabeledBitGraph -> Label -> UnlabeledBitGraph
successor cg node label = assert (node `elem` domain cg) $
  case label of
    Zero -> fst $ (successorMap cg Map.! node)
    One  -> snd $ (successorMap cg Map.! node)

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
           then let (zeroSucc,oneSucc) = succMap Map.! next
                    adder elem = if elem `Set.notMember` finites
                                    then insertSet elem
                                    else id
                    restPlus = adder zeroSucc $ adder oneSucc rest
             in computeWellfounded restPlus (Set.insert next finites)
           else computeWellfounded rest finites
    wellfounded = computeWellfounded [diagonal size] Set.empty
    nonWellfounded = (Map.keysSet succMap) Set.\\ wellfounded

relationOfWord :: Size -> CaleyGraph -> [Label] -> UnlabeledBitGraph
relationOfWord size cg word = Prelude.foldl (successor cg) (diagonal size) word

finiteWords :: Size -> CaleyGraph -> [([Label],UnlabeledBitGraph)]
finiteWords size cg = generateFiniteWords [([], diagonal size)] where
  generateFiniteWords [] = []
  generateFiniteWords ((nextWord, nextRel):rest) =
    let (zeroSucc,oneSucc) = successorMap cg Map.! nextRel
    in if nextRel `Set.member` wellfoundedElements cg
         then (reverse nextWord,nextRel) :
                     (generateFiniteWords ((Zero:nextWord,zeroSucc)
                                         : (One:nextWord,oneSucc) : rest))
         else generateFiniteWords rest

multiples :: Size -> UnlabeledBitGraph -> Set.Set UnlabeledBitGraph
multiples size rel = generateMultiples rel (Set.singleton rel) where
  generateMultiples r accum = let
      next = compose size r rel
    in if next `Set.member` accum
         then accum
         else generateMultiples next (Set.insert next accum)

isGood :: Size -> CaleyGraph -> Bool
isGood size cg = all (not . hasNoRefl size) (wellfoundedElements cg) &&
                 all (hasUniv size) (nonWellfoundedElements cg)

isGoodForDom :: Size -> CaleyGraph -> [Node] -> Bool
isGoodForDom size cg dom = all (not . hasNoReflInDom size dom) (wellfoundedElements cg) &&
                           all (hasUnivInDom size dom) (nonWellfoundedElements cg)

{-
 Should check that every finite word has a reflexive point that is universal in some multiple of the relation of that finite word.
-}

isPossibleValue :: Size -> CaleyGraph -> [Label] -> [Node] -> Node -> Bool
isPossibleValue size cg word others node =
  all (\v -> hasArc size (relationOfWord size cg word) (node,v)) others

