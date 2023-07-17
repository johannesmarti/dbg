module Examples.Plans (
  force3dPlan,
  force4dPlan,
  alloc3Plan
) where

import Plan
import Data.Label

force3dPlan :: Plan Char
force3dPlan =
  insert [Zero] (spoke 'a' []) $
  insert [One] (spoke 'b' []) $
  insert [Zero,One] (spoke 'a' [('b', 1)]) $
  insert [One,Zero] (spoke 'c' []) $
  insert [Zero,One,One] (spoke 'b' []) $
  insert [Zero,One,One,Zero] (spoke 'c' []) $
  insert [Zero,One,One,Zero,One] (spoke 'a' []) $
  insert [One,Zero,Zero] (spoke 'a' []) $
  insert [One,Zero,Zero,One] (spoke 'b' []) $
  insert [One,Zero,Zero,One,Zero] (spoke 'c' []) $ empty

force4dPlan :: Plan Int
force4dPlan =
  insert [Zero] (spoke 3 []) $
  insert [One] (spoke 0 []) $
  insert [Zero,One] (spoke 0 [(2, 1)]) $
  insert [One,Zero] (spoke 1 []) $
  insert [Zero,One,One] (spoke 0 []) $
  insert [Zero,One,One,Zero] (spoke 1 []) $
  insert [Zero,One,One,Zero,One] (spoke 0 []) $
  insert [One,Zero,Zero] (spoke 0 []) $
  insert [One,Zero,Zero,One] (spoke 0 []) $
  insert [One,Zero,Zero,One,Zero] (spoke 1 []) $
  insert [One,Zero,Zero,Zero] (spoke 3 []) $
  insert [One,Zero,Zero,Zero,One] (spoke 2 []) $
  insert [One,Zero,Zero,Zero,One,Zero] (spoke 1 []) $
  insert [One,Zero,Zero,Zero,One,Zero,Zero] (spoke 0 []) $ empty

alloc3Plan :: Plan Int
alloc3Plan =
  insert [Zero] (spoke 0 []) $
  insert [One] (spoke 1 []) $
  insert [Zero,One] (spoke 0 [(1, 2)]) $
  insert [One,Zero] (spoke 3 [(2, 1)]) $

  insert [Zero,One,One] (spoke 1 []) $
  insert [Zero,One,One,Zero] (spoke 2 []) $
  insert [Zero,One,One,Zero,One] (spoke 0 []) $

  insert [Zero,One,One,Zero,Zero] (spoke 1 []) $
  insert [Zero,One,One,Zero,Zero,One] (spoke 1 []) $
  insert [One,Zero,Zero,One,One] (spoke 1 []) $
  insert [One,Zero,Zero,One,One,Zero] (spoke 2 []) $

  insert [One,Zero,Zero] (spoke 1 []) $
  insert [One,Zero,Zero,One] (spoke 1 []) $
  insert [One,Zero,Zero,One,Zero] (spoke 2 []) $

  insert [One,Zero,Zero,Zero] (spoke 2 []) $
  insert [One,Zero,Zero,Zero,One] (spoke 0 []) $
  insert [One,Zero,Zero,Zero,One,Zero] (spoke 3 []) $
  insert [One,Zero,Zero,Zero,One,Zero,Zero] (spoke 1 []) $

  insert [One,Zero,Zero,Zero,Zero] (spoke 3 []) $
  insert [One,Zero,Zero,Zero,Zero,One] (spoke 1 []) $
  insert [One,Zero,Zero,Zero,Zero,One,Zero] (spoke 2 []) $
  insert [One,Zero,Zero,Zero,Zero,One,Zero,Zero] (spoke 1 []) $
  insert [One,Zero,Zero,Zero,Zero,One,Zero,Zero,Zero] (spoke 2 []) $

  insert [One,Zero,Zero,Zero,Zero,Zero] (spoke 0 []) $
  insert [One,Zero,Zero,Zero,Zero,Zero,One] (spoke 1 []) $
  insert [One,Zero,Zero,Zero,Zero,Zero,One,Zero] (spoke 2 []) $
  insert [One,Zero,Zero,Zero,Zero,Zero,One,Zero,Zero] (spoke 1 []) $
  insert [One,Zero,Zero,Zero,Zero,Zero,One,Zero,Zero,Zero] (spoke 2 []) $
  insert [One,Zero,Zero,Zero,Zero,Zero,One,Zero,Zero,Zero,Zero] (spoke 3 []) $
  empty
