module HomoForForce3d (
  game,
) where

import Lifting
import Game
import Patterns

ba = bn 'a'
bb = bn 'b'
bc = bn 'c'
sssa = si . si . si $ ba
sssb = si . si . si $ bb
sssc = si . si . si $ bc
dsssab = du sssa sssb
list4 = [dsssab]
--list5 = [dssss01,dssss02] ++ map deepen list4

game = easyGame 7 4 force3dI force3d
 [noop,
  noop,
  noop,
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

