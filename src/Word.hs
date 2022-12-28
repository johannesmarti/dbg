module Word (
  allWords,
  allWordsOfLength,
  turns,
  normalForm,
  isInNormalForm,
  repeatingInits,
  minimalRepeat,
  isDivisible,
  isBaseWord,
  realTurns,
  onAllTurns,
  turnForward,
  turnBackward,
) where

allWords :: [a] -> [[a]]
allWords alphabet = [] : generate [[]] where
  generate (next:rest) = let
      exts = map (\sym -> next ++ [sym]) alphabet
    in exts ++ (generate (rest ++ exts))

allWordsOfLength :: [a] -> Int -> [[a]]
allWordsOfLength _ 0 = [[]]
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
normalForm [] = []
normalForm w = minimum (turns w)

isInNormalForm :: Ord a => [a] -> Bool
isInNormalForm list = list == normalForm list

repeatingInits :: Eq a => [a] -> [[a]]
repeatingInits = map fst . filter isRepeat . splits where
  isRepeat (initial, rest) = eatThroughRest initial rest where
    eatThroughRest [] r = eatThroughRest initial r
    eatThroughRest _ [] = True
    eatThroughRest (i:is) (r:rs) =
      if i == r then eatThroughRest is rs
                else False

minimalRepeat :: Eq a => [a] -> [a]
minimalRepeat [] = []
minimalRepeat w = head (repeatingInits w)

isDivisible :: Eq a => [a] -> Bool
isDivisible w = let
    lenW = length w
    lenM = length (minimalRepeat w)
  in lenW /= lenM && lenW `rem` lenM == 0

isBaseWord :: Ord a => [a] -> Bool
isBaseWord w = isInNormalForm w && not (isDivisible w)

realTurns :: [a] -> [[a]]
realTurns l = turner l [] where
  turner [] _ = []
  turner (n:r) o = ((n:r) ++ reverse o) : turner r (n:o)

onAllTurns :: ([a] -> [b] -> c) -> [a] -> [b] -> [c]
onAllTurns f xs ys = map (uncurry f) (zip (realTurns xs) (realTurns ys))

turnForward :: [a] -> [a]
turnForward [] = error "empty list can not turn forward"
turnForward (f:rest) = rest ++ [f]

turnBackward :: [a] -> [a]
turnBackward [] = error "empty list can not turn forward"
turnBackward list = a:l' where
  (a,l') = worker list
  worker [a]   = (a,[])
  worker (n:l) = let (a,l') = worker l in (a,n:l')
