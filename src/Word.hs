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
  descent,
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

turnForward :: [a] -> [a]
turnForward [] = []
turnForward (f:rest) = rest ++ [f]

descent :: Eq a => [a] -> [a]
descent word = let
    letter = head word 
    nextInCycle = turnForward word
    candidates = repeatingInits nextInCycle
    goodCandidate c = last c /= letter
    goodCandidates = filter goodCandidate candidates
  in if isDivisible word
       then error "trying to descend from divisible word"
       else case goodCandidates of
              []  -> nextInCycle
              [d] -> d
              _   -> error "There is a word with multiple descends. This is conjectured to be impossible!"
