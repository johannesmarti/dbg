module HomoFor41430174 (
  game,
) where

import Lifting
import ConciseGraph
import Game

b0 = bn 0
b1 = bn 1
b2 = bn 2
b3 = bn 3
sss0 = si . si . si $ b0
sss1 = si . si . si $ b1
sss2 = si . si . si $ b2
sss3 = si . si . si $ b3
dsss13 = du sss1 sss3
list4 = [dsss13]
ssss0 = si $ sss0
ssss1 = si $ sss1
ssss2 = si $ sss2
ssss3 = si $ sss3
dssss01 = du ssss0 ssss1
dssss02 = du ssss0 ssss2
list5 = [dssss01,dssss02] ++ map deepen list4
list6 = map deepen list5
list7 = map deepen list6
sssssss0 = si . si . si $ ssss0
sssssss1 = si . si . si $ ssss1
sssssss2 = si . si . si $ ssss2
sssssss3 = si . si . si $ ssss3
dsssssss23 = du sssssss2 sssssss3
list8 = [dsssssss23] ++ map deepen list7
list9 = map deepen list8
list10 = map deepen list9
list11 = map deepen list10
list12 = map deepen list11
game = easyGame 12 (conciseGraphI 4) 41430174
 [noop,
  noop,
  noop,
  remove list4,
  remove list5 `nd` newReflIn 4 11422,
  remove list6 `nd` newReflIn 4 11422,
  remove list7 `nd` newReflIn 4 11422,
  remove list8 `nd` newReflIn 4 11422,
  remove list9 `nd` newReflIn 4 11422,
  remove list10 `nd` newReflIn 4 11422,
  remove list11 `nd` newReflIn 4 11422,
  remove list12 `nd` newReflIn 4 11422,
  noop]

