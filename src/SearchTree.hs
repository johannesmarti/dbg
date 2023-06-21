module SearchTree (
  SearchTreeGenerator(SearchTreeGenerator),
  SearchTree(SearchTreeNode),
  searchTreeFromGenerator,
  hotPathSearch,
  hotPathSearch',
  depthFirstSearch,
  depthFirstSearch',
  breadthFirstSearch,
  breadthFirstSearch',
  dovetailingSearch,
  dovetailingSearch',
) where

import Data.List.Extra (firstJust)

data SearchTreeGenerator d = SearchTreeGenerator {
  generateRoot     :: d,
  generateChildren :: d -> [d]
}

data SearchTree d = SearchTreeNode {
  label    :: d,
  children :: [SearchTree d]
}

searchTreeFromGenerator :: SearchTreeGenerator d -> SearchTree d
searchTreeFromGenerator generator = worker (generateRoot generator) where
  worker d = SearchTreeNode d (map worker (generateChildren generator d))

{-
Searches just along the left most path, that is the path that always follows
the first child. Any subtrees of younger siblings are not searched.
-}
hotPathSearch :: (d -> Bool) -> Int -> SearchTree d -> [d]
hotPathSearch predicate depth tree = if predicate dl then dl : t else t where
  dl = label tree
  t = if depth <= 0 then []
      else case children tree of
             []    -> []
             (h:_) -> hotPathSearch predicate (depth - 1) h

hotPathSearch' :: (d -> Bool) -> Int -> SearchTree d -> Maybe d
hotPathSearch' predicate depth tree = let dl = label tree
  in if predicate dl then Just dl
     else if depth <= 0 then Nothing
     else case children tree of
            []    -> Nothing
            (h:_) -> hotPathSearch' predicate (depth - 1) h

depthFirstSearch :: (d -> Bool) -> Int -> SearchTree d -> [d]
depthFirstSearch predicate depth tree = if predicate dl then dl : t else t where
  dl = label tree
  t = if depth <= 0 then []
      else concatMap (depthFirstSearch predicate (depth - 1)) (children tree)

depthFirstSearch' :: (d -> Bool) -> Int -> SearchTree d -> Maybe d
depthFirstSearch' predicate depth tree = let dl = label tree
  in if predicate dl then Just dl
     else if depth <= 0 then Nothing
     else firstJust (depthFirstSearch' predicate (depth - 1)) (children tree)

{-
It switches the order at every level. Thus in a binary tree we search in this
order r,0,1,10,11,00,01,010,011,001,001,110,111,100,101,1010,1011,1000,...
This is done to avoid reversing and appending at the end of worklists.
-}
breadthFirstSearch :: (d -> Bool) -> Int -> SearchTree d -> [d]
breadthFirstSearch predicate depth tree = worker depth [tree] [] where
  worker _ [] [] = []
  worker d [] nextLevel = if d <= 0 then []
                          else worker (d - 1) nextLevel []
  worker d (next:rest) nextLevel = if predicate dl then dl : t else t where
    dl = label next
    t  = worker d rest ((children next) ++ nextLevel)

breadthFirstSearch' :: (d -> Bool) -> Int -> SearchTree d -> Maybe d
breadthFirstSearch' predicate depth tree = worker depth [tree] [] where
  worker _ [] [] = Nothing
  worker d [] nextLevel = if d <= 0 then Nothing
                          else worker (d - 1) nextLevel []
  worker d (next:rest) nextLevel = let dl = label next
    in if predicate dl then Just dl
       else worker d rest ((children next) ++ nextLevel)

{-
Searches a tree
  (r,[(0, [(00,[...]), ...]),(1, [(10, [...]), ...]), (2, [...]), ...])
in the order r,0,00,1,000,01,10,2,0000,001,010,02,100,11,20,3,...
Is implemented with a worklist. In every step we are adding elements at the end
of the list. It would probabely be better to use some specialized fifo
datastructure as the worklist. Steps is the number of labels that is checked. A negative number leads to an infinte search.
-}
dovetailingSearch :: (d -> Bool) -> Int -> SearchTree d -> [d]
dovetailingSearch predicate steps tree = worker steps [[tree]] where
  worker 0 _ = []
  worker _ [] = []
  worker s ([]:rest) = worker s rest
  worker s ((next:siblings):rest) = if predicate dl then dl : t else t where
    dl = label next
    t = worker (s - 1) (rest ++ [children next, siblings])

dovetailingSearch' :: (d -> Bool) -> Int -> SearchTree d -> Maybe d
dovetailingSearch' predicate steps tree = worker steps [[tree]] where
  worker 0 _ = Nothing
  worker _ [] = Nothing
  worker s ([]:rest) = worker s rest
  worker s ((next:siblings):rest) = let dl = label next
    in if predicate dl then Just dl
       else worker (s - 1) (rest ++ [children next, siblings])
