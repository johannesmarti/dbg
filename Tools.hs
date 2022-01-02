module Tools (
  untilNothing,
  takeTill,
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
