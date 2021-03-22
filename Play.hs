{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
--{-# LANGUAGE MultiParamTypeClasses #-}

module Play (

) where

data Label = One | Zero

class Domain d x where
  elements :: d -> [x]

instance Domain () Label where
  elements _ = [One,Zero]

instance Domain Int Int where
  elements k = [0..(k - 1)]

class Domain d x => Graph g d x where
  domain :: g -> d
  successors :: g -> x -> [x]
  predecessors :: g -> x -> [x]

class (Domain d x, Domain ld l, Graph g d x) => LabelledGraph lg g d x ld l where
  atLabel :: lg -> l -> g
  labels :: lg -> ld
