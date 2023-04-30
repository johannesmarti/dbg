module SearchTree (
  SearchTreeGenerator(SearchTreeGenerator),
  SearchTree(SearchTreeNode),
  searchTreeFromGenerator,
  hotPathSearch,
  depthFirstSearch,
  breadthFirstSearch,
  dovetailingSearch,
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
hotPathSearch :: (d -> Bool) -> Int -> SearchTree d -> Maybe d
hotPathSearch predicate depth tree = let dl = label tree
  in if predicate dl then Just dl
     else if depth <= 0 then Nothing
     else case children tree of
            []    -> Nothing
            (h:_) -> hotPathSearch predicate (depth - 1) h

depthFirstSearch :: (d -> Bool) -> Int -> SearchTree d -> Maybe d
depthFirstSearch predicate depth tree = let dl = label tree
  in if predicate dl then Just dl
     else if depth <= 0 then Nothing
     else firstJust (depthFirstSearch predicate (depth - 1)) (children tree)

breadthFirstSearch :: (d -> Bool) -> Int -> SearchTree d -> Maybe d
breadthFirstSearch predicate depth tree = worker depth [tree] [] where
  worker _ [] [] = Nothing
  worker d [] nextLevel = if depth <= 0 then Nothing
                          else worker (d - 1) nextLevel []
  worker d (next:rest) nextLevel = let dl = label next
    in if predicate dl then Just dl
       else worker d rest ((children tree) ++ nextLevel)

{-
Searches a tree
  (r,[(0, [(00,[...]), ...]),(1, [(10, [...]), ...]), (2, [...]), ...])
in the order r,0,00,1,000,01,10,2,0000,001,010,02,100,11,20,3,...
Is implemented with a worklist. In every step we are adding elements at the end
of the list. It would probaely much better to use some specialized fifo
datastructure as the worklist.
-}
dovetailingSearch :: (d -> Bool) -> Int -> SearchTree d -> Maybe d
dovetailingSearch predicate steps tree = worker steps [[tree]] where
  worker 0 _ = Nothing
  worker _ [] = Nothing
  worker s ([]:rest) = worker s rest
  worker s ((next:siblings):rest) = let dl = label next
    in if predicate dl then Just dl
       else worker (s - 1) (rest ++ [children next, siblings])
