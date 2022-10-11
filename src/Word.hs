module Word (
  allWordsOfLength,
  turns,
  normalForm,
  isInNormalForm,
  repeatingInits,
  minimalRepeat,
) where

allWordsOfLength :: [a] -> Int -> [[a]]
allWordsOfLength list 0 = [[]]
{-
allWordsOfLength list n = concatMap f (allWordsOfLength list (n - 1)) where
  f shorterWord = map ( : shorterWord) list
-}
allWordsOfLength list n = concatMap f list where
  f letter = map (letter :) (allWordsOfLength list (n - 1))

splits :: [a] -> [([a],[a])]
splits xx = zipWith splitAt [1..(length xx)] (repeat xx)

turns :: [a] -> [[a]]
turns l =  map reassemble (splits l) where
  reassemble (x,y) = y ++ x

normalForm :: Ord a => [a] -> [a]
normalForm = minimum . turns

isInNormalForm :: Ord a => [a] -> Bool
isInNormalForm list = list == normalForm list

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
