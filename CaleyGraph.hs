module CaleyGraph (

) where

import Control.Exception.Base
import Data.List.Ordered
import Data.Map.Strict as Map
import Data.Set as Set

import qualified BitGraph as BG
import Label
import UnlabeledBitGraph

newtype CaleyGraph = CaleyGraph {
  successorMap :: Map.Map Word (Word,Word)
}

domain :: CaleyGraph -> Set Word
domain = keysSet . successorMap

succ :: CaleyGraph -> Word -> Label -> Word
succ cg node label = assert (node `elem` domain cg) $
  case label of
    Zero -> fst $ (successorMap cg ! node)
    One  -> snd $ (successorMap cg ! node)

rightCaleyGraph :: Int -> BG.BitGraph -> CaleyGraph
rightCaleyGraph size bitgraph =
  CaleyGraph (keepAdding [diagonal size] Map.empty) where
    zeroRel = BG.relationOfLabel size bitgraph Zero
    oneRel = BG.relationOfLabel size bitgraph One
    keepAdding [] m = m
    keepAdding (next:rest) m = let
      zeroSucc = compose size next zeroRel
      oneSucc = compose size next oneRel
      newM = Map.insert next (zeroSucc,oneSucc) m
      adder elem = if elem `Map.member` newM then id else insertSet elem
      newWorklist = adder zeroSucc $ adder oneSucc rest
        in keepAdding newWorklist newM

reflexivityCondition :: Int -> CaleyGraph -> Bool
reflexivityCondition size cg =
  all (not . hasNoRefl size) (domain cg)
