{-# LANGUAGE TemplateHaskell #-}
module Test.Graph.BFS where

import Language.Haskell.TH
import Data.Maybe
import Data.List
import Data.List.Unique
import Test.Test
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM
import Data.Maybe

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import Data.Graph
import Data.Graph.BFS
import Data.Graph.Grid
import Data.BlumeCapel
import Data.BlumeCapel.GSNetwork

fastTests :: [Test]
fastTests = [ 
              test2
            , test3
            ]

data TestGraph1 = TestGraph1
instance Eq TestGraph1 where
  (==) _ _ = True

instance Graph TestGraph1 where
  vertices g = [1..7]
  neighbors g 1 = [2,5,6]
  neighbors g 2 = [1,5,3]
  neighbors g 3 = [2,4]
  neighbors g 4 = [3,5,7]
  neighbors g 5 = [1,2,7,4,6]
  neighbors g 6 = [1,7]
  neighbors g 7 = [3,5,7]
  edges = edgesFromNeighbors

test2 :: Test
test2 = do
  let name = "Test bfs on TestGraph1"
      g = TestGraph1
      out = level $ bfs g 1
      expe = IM.fromList [(1,0),(2,1),(5,1),(6,1),(3,2),(4,2),(7,2)]
   in case  out == expe of
        True -> testPassed name "passed!"
        False -> testFailed name $ (,) (show expe) (show out)

test3 :: Test
test3 = do
  let name = "Test bfs and adjBFS should give the same results"
      g = TestGraph1
      out = bfs g 1
      expe = adjBFS g (adjacencyMap g) 1
   in case  out == expe of
        True -> testPassed name "passed!"
        False -> testFailed name $ (,) (show expe) (show out)
