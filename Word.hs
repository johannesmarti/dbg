module Word (
  turns,
  repeatingInits,
  minimalRepeat,
) where

splits :: [a] -> [([a],[a])]
splits xx = zipWith splitAt [1..(length xx)] (repeat xx)

turns :: [a] -> [[a]]
turns l =  map reassemble (splits l) where
  reassemble (x,y) = y ++ x

repeatingInits :: Eq a => [a] -> [[a]]
repeatingInits = map fst . filter isRepeat . splits where
  isRepeat (initial, rest) = eatThroughRest initial rest where
    eatThroughRest [] r = eatThroughRest initial r
    eatThroughRest i [] = True
    eatThroughRest (i:is) (r:rs) =
      if i == r then eatThroughRest is rs
                else False

minimalRepeat :: Eq a => [a] -> [a]
minimalRepeat = head . repeatingInits
