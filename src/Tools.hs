module Tools (
  untilNothing,
  takeTill,
  strictPairs,
) where

untilNothing :: (a -> Maybe a) -> a -> [a]
untilNothing f g = let
    val = f g
  in case val of
       Nothing -> []
       Just x  -> x : untilNothing f x

takeTill :: (a -> Bool) -> [a] -> [a]
takeTill p [] = []
takeTill p (x:xs) = if p x then [x] else x : takeTill p xs

strictPairs :: [x] -> [(x,x)]
strictPairs list = worker list [] where
  worker [] accum = accum
  worker (next:rest) accum = innerWorker next rest rest accum
  innerWorker elem [] rest accum = worker rest accum
  innerWorker elem (p:ps) rest accum = innerWorker elem ps rest ((elem, p):accum)
