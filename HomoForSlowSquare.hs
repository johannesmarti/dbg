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
{- It is a bit unclear weather dssac should be removed. It is dominated in successors but not in predecessors. -}
dssac = du ssa ssc
list3 = [dssac]
sssa =  si ba
sssb =  si bb
sssc =  si bc
sssd =  si bd
list5 = []
--list6 = [] ++ map deepen list5

game = easyGame 7 5 slowSquareI slowSquare
 [noop,
  noop,
  noop,
  noop,
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

