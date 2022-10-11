module Coding (
  Coding,
  fromAssoc,
  codeSet,
  encode,
  decode,
  domain,
  codes,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple (swap)

data Coding x y = Coding {
  encoder    :: Map.Map x y,
  decoder    :: Map.Map y x
}

fromAssoc :: (Ord x, Ord y) => [(x,y)] -> Coding x y
fromAssoc assoc = Coding enMap deMap where -- should check injectivity
  enMap = Map.fromList assoc
  deMap = Map.fromList (map swap assoc)

codeSet :: Ord x => Set.Set x -> Coding x Int
codeSet set = fromAssoc assoc where
  assoc = zip (Set.toList set) [0 .. ]

cautiousEncode :: Ord x => Coding x y -> x -> Maybe y
cautiousEncode coding node = Map.lookup node (encoder coding)

cautiousDecode :: Ord y => Coding x y -> y -> Maybe x
cautiousDecode coding code = Map.lookup code (decoder coding)

encode :: Ord x => Coding x y -> x -> y
encode coding node =
  Map.findWithDefault (error "node not in domain") node (encoder coding) 

decode :: Ord y => Coding x y -> y -> x
decode coding code =
  Map.findWithDefault (error "object not an code") code (decoder coding)

domain :: Ord x => Coding x y -> Set.Set x
domain coding = Map.keysSet (encoder coding)

codes :: Ord y => Coding x y -> Set.Set y
codes coding = Map.keysSet (decoder coding)

