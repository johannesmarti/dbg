module Main (
  main
) where

import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import qualified Data.Vector as V
import System.Environment

import Patterns
import Plan
import Label
import LabeledGraph
import LiftedGraph


main :: IO ()
main = do
  let s001 = spoke 'a' [('b', 1), ('c', 2)]
  let s010 = spoke 'c' [('a', 1), ('b', 3)]
  let s100 = spoke 'b' [('a', 1), ('c', 2)]
  let spokes = V.fromList [s001, s010, s100]
  let lg = LiftedGraph.fromLGraph force3dI force3d
  let (fatVec, extendedLg) = runState (wrapSpiral spokes) lg
  let ig = graph extendedLg
  let f001 = fatVec V.! 0
  let f010 = fatVec V.! 1
  let f100 = fatVec V.! 2
  let ha = hasArc intGraphI ig
  putStrLn $ unlines (prettyLiftedGraph extendedLg)
