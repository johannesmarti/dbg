module HomoFor2063974806 (
  game,
) where

import Lifting
import ConciseGraph
import Game

b0 = bn 0
b1 = bn 1
b2 = bn 2
b3 = bn 3
d03 = du b0 b3
d12 = du b1 b2
dd12 = deepen d12
ddd12 = deepen dd12
dddd12 = deepen ddd12
sss0 = si . si . si $ b0
sss1 = si . si . si $ b1
sss2 = si . si . si $ b2
sss3 = si . si . si $ b3
ssb03 = si . si $ d03
dsss01 = du sss0 sss1
dsss02 = du sss0 sss2
dsss2ssb03 = du sss2 ssb03
s0 = si b0
s1 = si b1
s2 = si b2
s3 = si b3
ds13 = du s1 s3
sds13 = si ds13
dsss2sds13 = du sss2 sds13
ss0 = si s0
ss2 = si s2
ss3 = si s3
dss02 = du ss0 ss2
dsss3dss02 = du sss3 dss02
ssd03 = si . si $ d03
dssd03dss02 = du ssd03 dss02
list4 = [dddd12,dsss01,dsss02,dsss2ssb03,dsss2sds13,dsss3dss02,dssd03dss02]
ssss2 = si . si $ ss2
ssss3 = si . si $ ss3
dssss23 = du ssss2 ssss3
list5 = [dssss23] ++ map deepen list4
list6 = map deepen list5
list7 = map deepen list6
list8 = map deepen list7
list9 = map deepen list8
game = easyGame 9 (conciseGraphI 4) 2063974806 [remove [d12],
  remove [dd12],
  remove [ddd12],
  remove list4,
  remove list5 `nd` newReflIn 4 49558,
  remove list6 `nd` newReflIn 4 49558,
  remove list7 `nd` newReflIn 4 49558,
  remove list8 `nd` newReflIn 4 49558,
  remove list9 `nd` newReflIn 4 49558 ]

