module Examples.Plans (
  force3dPlan,
  force4dPlan,
  alloc3Plan
) where

import Plans.CoveringGraph (lookupAddress)
import Plans.Spoke hiding (insert)
import Plans.Plan
import Data.Label

la = lookupAddress

force3dPlan :: Plan Char
force3dPlan =
  insert (la [Zero]) (spoke 'a' []) $
  insert (la [One]) (spoke 'b' []) $
  insert (la [Zero,One]) (spoke 'a' [('b', 1)]) $
  insert (la [One,Zero]) (spoke 'c' []) $
  insert (la [Zero,One,One]) (spoke 'b' []) $
  insert (la [Zero,One,One,Zero]) (spoke 'c' []) $
  insert (la [Zero,One,One,Zero,One]) (spoke 'a' []) $
  insert (la [One,Zero,Zero]) (spoke 'a' []) $
  insert (la [One,Zero,Zero,One]) (spoke 'b' []) $
  insert (la [One,Zero,Zero,One,Zero]) (spoke 'c' []) $ empty

force4dPlan :: Plan Int
force4dPlan =
  insert (la [Zero]) (spoke 3 []) $
  insert (la [One]) (spoke 0 []) $
  insert (la [Zero,One]) (spoke 0 [(2, 1)]) $
  insert (la [One,Zero]) (spoke 1 []) $
  insert (la [Zero,One,One]) (spoke 0 []) $
  insert (la [Zero,One,One,Zero]) (spoke 1 []) $
  insert (la [Zero,One,One,Zero,One]) (spoke 0 []) $
  insert (la [One,Zero,Zero]) (spoke 0 []) $
  insert (la [One,Zero,Zero,One]) (spoke 0 []) $
  insert (la [One,Zero,Zero,One,Zero]) (spoke 1 []) $
  insert (la [One,Zero,Zero,Zero]) (spoke 3 []) $
  insert (la [One,Zero,Zero,Zero,One]) (spoke 2 []) $
  insert (la [One,Zero,Zero,Zero,One,Zero]) (spoke 1 []) $
  insert (la [One,Zero,Zero,Zero,One,Zero,Zero]) (spoke 0 []) $ empty

alloc3Plan :: Plan Int
alloc3Plan =
  insert (la [Zero]) (spoke 0 []) $
  insert (la [One]) (spoke 1 []) $
  insert (la [Zero,One]) (spoke 0 [(1, 2)]) $
  insert (la [One,Zero]) (spoke 3 [(2, 1)]) $

  insert (la [Zero,One,One]) (spoke 1 []) $
  insert (la [Zero,One,One,Zero]) (spoke 2 []) $
  insert (la [Zero,One,One,Zero,One]) (spoke 0 []) $

  insert (la [Zero,One,One,Zero,Zero]) (spoke 1 []) $
  insert (la [Zero,One,One,Zero,Zero,One]) (spoke 1 []) $
  insert (la [One,Zero,Zero,One,One]) (spoke 1 []) $
  insert (la [One,Zero,Zero,One,One,Zero]) (spoke 2 []) $

  insert (la [One,Zero,Zero]) (spoke 1 []) $
  insert (la [One,Zero,Zero,One]) (spoke 1 []) $
  insert (la [One,Zero,Zero,One,Zero]) (spoke 2 []) $

  insert (la [One,Zero,Zero,Zero]) (spoke 2 []) $
  insert (la [One,Zero,Zero,Zero,One]) (spoke 0 []) $
  insert (la [One,Zero,Zero,Zero,One,Zero]) (spoke 3 []) $
  insert (la [One,Zero,Zero,Zero,One,Zero,Zero]) (spoke 1 []) $

  insert (la [One,Zero,Zero,Zero,Zero]) (spoke 3 []) $
  insert (la [One,Zero,Zero,Zero,Zero,One]) (spoke 1 []) $
  insert (la [One,Zero,Zero,Zero,Zero,One,Zero]) (spoke 2 []) $
  insert (la [One,Zero,Zero,Zero,Zero,One,Zero,Zero]) (spoke 1 []) $
  insert (la [One,Zero,Zero,Zero,Zero,One,Zero,Zero,Zero]) (spoke 2 []) $

  insert (la [One,Zero,Zero,Zero,Zero,Zero]) (spoke 0 []) $
  insert (la [One,Zero,Zero,Zero,Zero,Zero,One]) (spoke 1 []) $
  insert (la [One,Zero,Zero,Zero,Zero,Zero,One,Zero]) (spoke 2 []) $
  insert (la [One,Zero,Zero,Zero,Zero,Zero,One,Zero,Zero]) (spoke 1 []) $
  insert (la [One,Zero,Zero,Zero,Zero,Zero,One,Zero,Zero,Zero]) (spoke 2 []) $
  insert (la [One,Zero,Zero,Zero,Zero,Zero,One,Zero,Zero,Zero,Zero]) (spoke 3 []) $
  empty
