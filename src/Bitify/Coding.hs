module Bitify.Coding (
  Coding,
  domain,
  codes,
  encode,
  decode,
  encodeSet,
  decodeSet,
  encodeArcs,
  decodeArcs,
  encodeArc,
  decodeArc,
  identityCoding,
  codeSet,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple (swap)

data Coding x y = Coding {
  domain    :: Set.Set x,
  codes     :: Set.Set y,
  encode    :: x -> y,
  decode    :: y -> x,
  encodeSet :: Set.Set x -> Set.Set y,
  decodeSet :: Set.Set y -> Set.Set x,
  encodeArcs :: [(x,x)] -> [(y,y)],
  decodeArcs :: [(y,y)] -> [(x,x)]
}

pairMap :: (x -> y) -> (x,x) -> (y,y)
pairMap f (a,b) = (f a, f b)

encodeArc :: Coding x y -> (x,x) -> (y,y)
encodeArc c = pairMap (encode c)

decodeArc :: Coding x y -> (y,y) -> (x,x)
decodeArc c = pairMap (decode c)

identityCoding :: Set.Set x -> Coding x x
identityCoding set = Coding set set id id id id id id

fromAssoc :: (Ord x, Ord y) => [(x,y)] -> Coding x y
fromAssoc assoc = coding where -- should check injectivity
  enMap = Map.fromList assoc
  deMap = Map.fromList (map swap assoc)
  enc o = Map.findWithDefault (error "object not in domain of coding") o enMap
  dec c = Map.findWithDefault (error "object not a code of coding") c deMap
  coding = Coding (Map.keysSet enMap) (Map.keysSet deMap)
                  enc dec
                  (Set.map enc) (Set.map dec)
                  (map (pairMap enc)) (map (pairMap dec))

codeSet :: Ord x => Set.Set x -> Coding x Int
codeSet set = fromAssoc assoc where
  assoc = zip (Set.toList set) [0 .. ]
