module Plans.CoveringGraph (
  CoveringNode,
  turningWord,parent,fullPathDown,
  epsilon,zero,one,
  predecessor,
  properlyAscendingPredecessor,
  address,
  lookupAddress,
  generateNodes, cycles,
  cycleOfNode,
  isDescending,
  childCycles,
) where

import Control.Exception.Base
import Data.Function (fix)

import Data.Label
import Data.Path as Path
import Data.ListWord
import Data.WordTree

data CoveringNode = CoveringNode {
  turningWord :: [Label], -- turning word is believed to determine all other members
  address     :: [Label], -- address determines all other data members
  parent      :: CoveringNode,
  pathDown    :: Path CoveringNode
}

descent :: CoveringNode -> CoveringNode
descent node = Path.start (pathDown node)

deepEqual :: CoveringNode -> CoveringNode -> Bool
deepEqual (CoveringNode twa aa paa pda) (CoveringNode twb ab pab pdb) =
  if null twa then null twb
  else if null twb then False
  else twa == twb &&
       deepEqual paa pab &&
       pda == pdb -- actually we should call deep equal recursively here. but that is too much of a hassle

instance Eq CoveringNode where
  ea == eb = if turningWord ea == turningWord eb
               then assert (deepEqual ea eb)
                           True
               else False

instance Ord CoveringNode where
  ea `compare` eb = turningWord ea `compare` turningWord eb

instance Show CoveringNode where
  show (CoveringNode tw _ par _) = "[ " ++ prettyWord tw ++ " | " ++
                                    prettyWord (turningWord par) ++ " ]"

epsilon :: CoveringNode
epsilon = CoveringNode [] [] epsilon (There epsilon)

zero :: CoveringNode
zero = CoveringNode [Zero] [Zero] epsilon (There epsilon)

one :: CoveringNode
one = CoveringNode [One] [One] epsilon (There epsilon)

isAncestor :: CoveringNode -> CoveringNode -> Bool
isAncestor curr stack =
  if curr == stack then True
  else if stack == epsilon then False
  else isAncestor curr (parent stack)

pathToAncestor :: CoveringNode -> Path CoveringNode -> [Label]
pathToAncestor parentStack (Step curr label cont) =
  if curr `isAncestor` parentStack
    then []
    else label : (pathToAncestor parentStack cont)
pathToAncestor ps (There here) = assert (here == epsilon) $
                                 assert (here `isAncestor` ps) $ []

typeLabel :: CoveringNode -> Label
typeLabel n = if n == epsilon then error "epsilon has no type label"
                              else first (turningWord n)

fullPathDown :: CoveringNode -> Path CoveringNode
fullPathDown expd = Step expd (typeLabel expd) (pathDown expd)

data AscentStatus = ProperAscent | LoopingAscent | LoopBack
  deriving Eq

isAscent :: AscentStatus -> Bool
isAscent ProperAscent = True
isAscent LoopingAscent = True
isAscent LoopBack = False

type Expander = Label -> CoveringNode -> (CoveringNode, AscentStatus)

looper :: Expander -> Expander
looper expander label expd = let
    movedParent = fst $ (expander label) (parent expd)
    pathToAncestors = label : (pathToAncestor movedParent extendedPath)
    extendedPath = fullPathDown expd
    newAddress = address expd ++ [label]
  in if expd == epsilon then case label of Zero -> (zero, ProperAscent)
                                           One  -> (one,  ProperAscent)
     else if last (turningWord expd) == label
       then let turned = turnBackward (turningWord expd)
            in if turned == turningWord movedParent
                 then (movedParent, LoopBack)
                 else (CoveringNode turned newAddress movedParent extendedPath,
                       LoopingAscent)
     else assert (pathToAncestors /= turningWord movedParent) $
                 (CoveringNode pathToAncestors newAddress
                 	          movedParent extendedPath, ProperAscent)

fixCover :: Expander
fixCover = fix looper

treeCover :: Expander
treeCover l node = atAddress ((address node) ++ [l])

atAddress :: [Label] -> (CoveringNode, AscentStatus)
atAddress = labelOfWord cacheTree

cacheTree :: WordTree (CoveringNode, AscentStatus)
cacheTree =  wordTreeFromGenerator generator where
  expandOn l n = looper treeCover l (fst n)
  generator = WordTreeGenerator (epsilon, LoopingAscent)
                (expandOn Zero)
                (expandOn One)

lookupAddress :: [Label] -> CoveringNode
lookupAddress = fst . atAddress

predecessor :: Label -> CoveringNode -> CoveringNode
predecessor l n = fst $ treeCover l n

bothPredecessors :: CoveringNode -> ((CoveringNode, AscentStatus),
                                     (CoveringNode, AscentStatus))
bothPredecessors n = (label zeroTreeNode, label oneTreeNode) where
  treeNode = subtreeOfWord cacheTree (address n)
  zeroTreeNode = zeroSucc treeNode
  oneTreeNode  = oneSucc treeNode

loopingPredecessor :: CoveringNode -> CoveringNode
loopingPredecessor node = let
    ((pz,sz),(po,so)) = bothPredecessors node
  in if sz /= ProperAscent then pz
     else if so /= ProperAscent then po
     else error "Node has no looping predecessor. (This is believed to impossible)"

properlyAscendingPredecessor :: CoveringNode -> CoveringNode
properlyAscendingPredecessor node = let
    ((pz,sz),(po,so)) = bothPredecessors node
  in if sz == ProperAscent then pz
     else if so == ProperAscent then po
     else error "Node has no properly ascending predecessor. (This is believed to impossible)"

generateNodes :: [CoveringNode]
generateNodes = generator [epsilon] []

generator :: [CoveringNode] -> [CoveringNode] -> [CoveringNode]
generator [] [] = error "The covering graph seems to be finite. This is thought to be false!"
generator [] nextLevel = generator nextLevel []
generator (curr:rest) nextLevel = curr : generator rest (more ++ nextLevel) where
  pre = map ((flip treeCover) curr) labelsList
  more = map fst (filter (isAscent . snd) pre)

{- Returns an infinite list that contains one element from every cycle. -}
cycles :: [CoveringNode]
cycles = filter (isInNormalForm . turningWord) (tail generateNodes)

cycleOfNode :: CoveringNode -> [CoveringNode]
cycleOfNode node = (node : remaining) where
  infList = tail (iterate loopingPredecessor node)
  remaining = takeWhile (/= node) infList

{- True if the node is ascending somewhere. -}
isDescending :: CoveringNode -> Bool
isDescending node = let
    (_,status) = atAddress (address node)
  in case status of
       ProperAscent  -> True
       LoopingAscent -> False
       LoopBack      -> error "Node is looping back at its own address. This is thought to be impossible!"

{-
{-
 Maybe the recursion in the following function should be on the ``actual'' children of the descent point. The other nodes on the cycle don't really seem to matter that much.
-}
{- This is almost the converse of the parent relation. The first argument is a predicate which should be true on a connected subtree (over the tree of addresses) which contains the second argument. Only children inside of the subTree are returned. -}
children :: (CoveringNode -> Bool) -> CoveringNode -> [CoveringNode]
children inConnSubtree node = let
    -- TODO: Don't forget to filter by connSubtree
    cycle = cycleOfNode node
    descending = filter isDescending cycle
    childrenAtDescend descendNode = let
        -- Here we shouldprobabely take the children of the descent point!
        myChildren = children inConnSubtree descendNode
        startChainAtChild child = undefined {-
                move backwards from child along cycle (in the right way).
                But some conditions need to be satisfied, otherwise we just kepe cycling at the child and we don't get nodes whose parents is quite right?!?
            -}
      in concatMap startChainAtChild myChildren
  in if node == epsilon
       then [zero, one]
       else concatMap childrenAtDescend descending

{-
Assume we have the cycle of the parent

Assume we have all the children of a descending ancestor. It's only at these where children for the cycle can attach.
-}
-}

-- TODO: is it descendNode or descentNode
childCycles :: (CoveringNode -> Bool) -> CoveringNode -> [[CoveringNode]]
childCycles inConnSubtree node = let
    myCycle = cycleOfNode node
    descending = filter isDescending myCycle
    chainAt labels node = let infChain = scanl (flip predecessor) node labels
                            in takeWhile inConnSubtree infChain
    childrenAtDescend :: CoveringNode -> [[CoveringNode]]
    childrenAtDescend descendNode = let
        descentTarget = descent descendNode
        descentLabel = typeLabel descendNode
        candidates = concat (childCycles inConnSubtree descentTarget)
        interestingChild c = parent c == descentTarget
                                && not (c `elem` myCycle)
        myChildren = filter interestingChild candidates
        reverseLoop = cycle . reverse . turningWord $ descendNode
        startChainAtChild :: CoveringNode -> [CoveringNode]
        startChainAtChild child = let
            start = predecessor descentLabel child
          in {-trace ("starting chain at child: " ++ prettyWord (turningWord child) ++ " working for: " ++ prettyWord (turningWord descendNode)) $-}  chainAt reverseLoop start
            {-
                move backwards from child along cycle (in the right way).
                But some conditions need to be satisfied, otherwise we just kepe cycling at the child and we don't get nodes whose parents is quite right?!?
            -}
      in filter (not . null) (map startChainAtChild myChildren)
  in if node == epsilon then error "no child cycles for epsilon"
     else if node == zero then [chainAt (repeat Zero) (predecessor Zero one)]
     else if node == one  then [chainAt (repeat One) (predecessor One zero)]
     else concatMap childrenAtDescend descending


