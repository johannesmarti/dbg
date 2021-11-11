module Coding (
  Coding,
  aggressiveEncode,
  aggressiveDecode,
  domain,
  codes,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

data Coding x y = Coding {
  encoder    :: Map.Map x y,
  decoder    :: Map.Map y x
}

encode :: Ord x => Coding x y -> x -> Maybe y
encode coding node = Map.lookup node (encoder coding)

decode :: Ord y => Coding x y -> y -> Maybe x
decode coding code = Map.lookup code (decoder coding)

aggressiveEncode :: Ord x => Coding x y -> x -> y
aggressiveEncode coding node =
  Map.findWithDefault (error "node not in domain") node (encoder coding) 

aggressiveDecode :: Ord y => Coding x y -> y -> x
aggressiveDecode coding code =
  Map.findWithDefault (error "object not an code") code (decoder coding)

domain :: Ord x => Coding x y -> Set.Set x
domain coding = Map.keysSet (encoder coding)

codes :: Ord y => Coding x y -> Set.Set y
codes coding = Map.keysSet (decoder coding)

