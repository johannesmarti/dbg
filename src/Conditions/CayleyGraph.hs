module Conditions.CayleyGraph (
  CayleyGraph(..),
  domain,
  relationCacher,
  rightCayleyGraph,
  allWords,
  finiteWords,
  weakPathCondition,
  pathCondition,
  limitedPathCondition,
  printNodeWithSuccs,
  prettyCayleyGraph,
  Conditions.CayleyGraph.relationOfWord,
) where

import Control.Exception.Base
import Data.List.Ordered
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Label
import Graphs.BitGraph as BG
import Graphs.PairGraph
import Bitify.Bitifier
import Bitify.Coding hiding (domain)
import GraphTools.RelationCache

data CayleyGraph = CayleyGraph {
  successorMap :: Map.Map BitGraph (BitGraph,BitGraph),
  -- nonWellfoundedElements is the subset of the domain that is hit by infinitely many words over {Zero, One}.
  wellfoundedElements :: Set.Set BitGraph,
  nonWellfoundedElements :: Set.Set BitGraph
} deriving Show

relationCacher :: Bitification x -> CayleyGraph -> RelationCache BitGraph x
relationCacher bf cg = cache where
  c = coding bf
  s = numBits bf
  cache = RelationCache (relationInterface bf)
                        (decodeSet c . BG.reflexivesUnivInMultiple s)
                        (Conditions.CayleyGraph.relationOfWord s cg)

domain :: CayleyGraph -> Set.Set BitGraph
domain = Map.keysSet . successorMap

successor :: CayleyGraph -> BitGraph -> Label -> BitGraph
successor cg node label = assert (node `elem` domain cg) $
  case label of
    Zero -> fst $ (successorMap cg Map.! node)
    One  -> snd $ (successorMap cg Map.! node)

rightCayleyGraph :: Bitification x -> CayleyGraph
rightCayleyGraph bf =
  CayleyGraph succMap wellfounded nonWellfounded where
    size =  numBits bf
    lbg = labeledBitGraph bf
    zeroRel = zeroGraph lbg
    oneRel = oneGraph lbg
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

relationOfWord :: Size -> CayleyGraph -> [Label] -> BitGraph
relationOfWord size cg word = Prelude.foldl (successor cg) (diagonal size) word

allWords :: Size -> CayleyGraph -> [([Label],BitGraph)]
allWords size cg = generateAllWords [([], diagonal size)] where
  generateAllWords [] = []
  generateAllWords ((nextWord, nextRel):rest) =
    let (zeroSucc,oneSucc) = successorMap cg Map.! nextRel
    in (reverse nextWord,nextRel) :
                 (generateAllWords (rest ++ [(Zero:nextWord,zeroSucc),
                                             (One:nextWord,oneSucc)]))

finiteWords :: Size -> CayleyGraph -> [([Label],BitGraph)]
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
part of the CayleyGraph -}
pathCondition :: Size -> CayleyGraph -> Bool
pathCondition size cg = all (hasUniv size) (nonWellfoundedElements cg) &&
  all (\rel -> rel == diagonal size || hasReflAndUnivInMultiple size rel) (wellfoundedElements cg) &&
  all (\n -> any (\r -> isUniv size r n) (domain cg)) (nodes size)

weakPathCondition :: Size -> CayleyGraph -> Bool
weakPathCondition size cg = all (hasUniv size) (nonWellfoundedElements cg) &&
  all (\rel -> rel == diagonal size || hasReflAndUnivInMultiple size rel) (wellfoundedElements cg)

limitedPathCondition :: Size -> Int -> CayleyGraph -> Bool
limitedPathCondition size cutoff cg = let
  rels = take cutoff $ map snd $ allWords size cg
  dia = diagonal size
  isOk rel = rel == dia || hasReflAndUnivInMultiple size rel
    in all isOk rels

printNodeWithSuccs :: CayleyGraph -> BitGraph -> String
printNodeWithSuccs cg node = show node ++ " -> " ++ show succs where
  succs = (successorMap cg) Map.! node

prettyCayleyGraph :: CayleyGraph -> [String]
prettyCayleyGraph cg = map printNode (Map.toList (successorMap cg)) where
  printNode (node,succs) = show node ++ " -> " ++ show succs
