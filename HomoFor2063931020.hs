module HomoFor2063931020 (
  game,
) where

import Lifting
import Game
import ConciseGraph

b0 = bn 0
b1 = bn 1
b2 = bn 2
b3 = bn 3
sss0 =  si . si . si $ b0
sss1 =  si . si . si $ b1
sss2 =  si . si . si $ b2
sss3 =  si . si . si $ b3
{- It is a bit unclear weather dssac should be removed. It is dominated in successors but not in predecessors. -}
dsss03 = du sss0 sss3
list4 = [dsss03]
list5 = [] ++ map deepen list4

game = easyGame 7 9 (conciseGraphI 4) 2063931020
 [noop,
  noop,
  noop,
  remove list4,
  remove list5,
{-  remove list4,
  remove list5 `nd` newReflIn 4 11422,
  remove list6 `nd` newReflIn 4 11422,
  remove list7 `nd` newReflIn 4 11422,
  remove list8 `nd` newReflIn 4 11422,
  remove list9 `nd` newReflIn 4 11422,
  remove list10 `nd` newReflIn 4 11422,
  remove list11 `nd` newReflIn 4 11422,
  remove list12 `nd` newReflIn 4 11422,-}
  noop]

