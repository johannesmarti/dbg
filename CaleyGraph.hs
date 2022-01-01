module CaleyGraph (
  CaleyGraph(..),
  domain,
  relationOfWord,
  rightCaleyGraph,
  allWords,
  finiteWords,
  isGood,
  isPossibleValue,  
  printNodeWithSuccs,
  prettyCaleyGraph,
) where

import Control.Exception.Base
import Data.List.Ordered
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Debug.Trace

import Label
import BitGraph

data CaleyGraph = CaleyGraph {
  successorMap :: Map.Map BitGraph (BitGraph,BitGraph),
  -- nonWellfoundedElements is the subset of the domain that is hit by infinitely many words over {Zero, One}.
  wellfoundedElements :: Set.Set BitGraph,
  nonWellfoundedElements :: Set.Set BitGraph
} deriving Show

domain :: CaleyGraph -> Set.Set BitGraph
domain = Map.keysSet . successorMap

successor :: CaleyGraph -> BitGraph -> Label -> BitGraph
successor cg node label = assert (node `elem` domain cg) $
  case label of
    Zero -> fst $ (successorMap cg Map.! node)
    One  -> snd $ (successorMap cg Map.! node)

rightCaleyGraph :: Size -> (BitGraph,BitGraph) -> CaleyGraph
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

relationOfWord :: Size -> CaleyGraph -> [Label] -> BitGraph
relationOfWord size cg word = Prelude.foldl (successor cg) (diagonal size) word

allWords :: Size -> CaleyGraph -> [([Label],BitGraph)]
allWords size cg = generateAllWords [([], diagonal size)] where
  generateAllWords [] = []
  generateAllWords ((nextWord, nextRel):rest) =
    let (zeroSucc,oneSucc) = successorMap cg Map.! nextRel
    in (reverse nextWord,nextRel) :
                 (generateAllWords (rest ++ [(Zero:nextWord,zeroSucc),
                                             (One:nextWord,oneSucc)]))

finiteWords :: Size -> CaleyGraph -> [([Label],BitGraph)]
finiteWords size cg = generateFiniteWords [([], diagonal size)] where
  generateFiniteWords [] = []
  generateFiniteWords ((nextWord, nextRel):rest) =
    let (zeroSucc,oneSucc) = successorMap cg Map.! nextRel
    in if nextRel `Set.member` wellfoundedElements cg
         then (reverse nextWord,nextRel) :
                     (generateFiniteWords ((Zero:nextWord,zeroSucc)
                                         : (One:nextWord,oneSucc) : rest))
         else generateFiniteWords rest

{- These would probabely be much quicker if we would cache the multiples as
part of the CaleyGraph -}
isGood :: Size -> CaleyGraph -> Bool
isGood size cg = all (hasUniv size) (nonWellfoundedElements cg) &&
  all (\rel -> rel == diagonal size || hasReflAndUnivInMultiple size rel) (wellfoundedElements cg) &&
  all (\n -> any (\r -> isUniv size r n) (domain cg)) (nodes size)

splits :: [a] -> [([a],[a])]
splits xx = zipWith splitAt [1..(length xx)] (repeat xx)

repeatingInits :: Eq a => [a] -> [[a]]
repeatingInits = map fst . filter isRepeat . splits where
  isRepeat (initial, rest) = eatThroughRest initial rest where
    eatThroughRest [] r = eatThroughRest initial r
    eatThroughRest i [] = True
    eatThroughRest (i:is) (r:rs) =
      if i == r then eatThroughRest is rs
                else False

isPossibleValue :: Size -> CaleyGraph -> [Label] -> Node -> Bool
isPossibleValue size cg word node =
  all (\v -> hasBitForArc size (relationOfWord size cg word) (node,v)) (nodes size) &&
  all (\i -> hasBitForArc size (relationOfWord size cg i) (node,node)) (repeatingInits word)

printNodeWithSuccs :: CaleyGraph -> BitGraph -> String
printNodeWithSuccs cg node = show node ++ " -> " ++ show succs where
  succs = (successorMap cg) Map.! node

prettyCaleyGraph :: CaleyGraph -> [String]
prettyCaleyGraph cg = map printNode (Map.toList (successorMap cg)) where
  printNode (node,succs) = show node ++ " -> " ++ show succs
