module CaleyGraph (

) where

import Control.Exception.Base
import Data.List.Ordered
import Data.Map.Strict as Map
import Data.Set as Set

import qualified BitGraph as BG
import Label
import UnlabeledBitGraph

data CaleyGraph = CaleyGraph {
  successorMap :: Map.Map Word (Word,Word),
  -- nonWellfoundedElements is the subset of the domain that is hit by infinitely many words over Zero One.
  wellfoundedElements :: Set.Set Word,
  nonWellfoundedElements :: Set.Set Word
} deriving Show

domain :: CaleyGraph -> Set Word
domain = keysSet . successorMap

succ :: CaleyGraph -> Word -> Label -> Word
succ cg node label = assert (node `elem` domain cg) $
  case label of
    Zero -> fst $ (successorMap cg ! node)
    One  -> snd $ (successorMap cg ! node)

rightCaleyGraph :: Int -> BG.BitGraph -> CaleyGraph
rightCaleyGraph size bitgraph =
  CaleyGraph succMap wellfounded nonWellfounded where
    zeroRel = BG.relationOfLabel size bitgraph Zero
    oneRel = BG.relationOfLabel size bitgraph One
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


reflexivityCondition :: Int -> CaleyGraph -> Bool
reflexivityCondition size cg =
  all (not . hasNoRefl size) (domain cg)
