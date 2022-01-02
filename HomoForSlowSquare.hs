module HomoForSlowSquare (
  game,
) where

import Lifting
import Game
import Patterns

ba = bn 'a'
bb = bn 'b'
bc = bn 'c'
bd = bn 'd'
ssa =  si . si $ ba
ssb =  si . si $ bb
ssc =  si . si $ bc
ssd =  si . si $ bd
{- remove dssac because it is dominated in one by la la and in zero by blabla.
   It is possible to be dominated by different existing nodes in different
   colors. -}
dssac = du ssa ssc
list3 = [dssac]
sssa =  si ba
sssb =  si bb
sssc =  si bc
sssd =  si bd
dsssbc = du sssb sssc
dsssbd = du sssb sssd
list4 = [dsssbc,dsssbd] ++ map deepen list3
list5 = [] ++ map deepen list4

game = easyGame 7 4 slowSquareI slowSquare
 [noop,
  noop,
  remove list3,
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

