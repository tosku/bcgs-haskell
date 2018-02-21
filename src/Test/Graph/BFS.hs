{-# LANGUAGE TemplateHaskell #-}
module Test.Graph.BFS where

import Language.Haskell.TH
import Data.Maybe
import Data.List
import Data.List.Unique
import Test.BCTest
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
            {-, test3-}
            ]

graphTest1 = Graph { vertices = [1..7]
                   , neighbors = (\v ->let nei 1 = [2,5,6]
                                           nei 2 = [5,3]
                                           nei 3 = [4]
                                           nei 4 = []
                                           nei 5 = [4,7]
                                           nei 6 = [7]
                                           nei 7 = [4]
                                       in nei v
                                 )
                   , edges = edgesFromNeighbors graphTest1
                   , edgeIndex = mapEdgeIndx graphTest1
                   , outEdges = (\v -> map (\n -> Edge v n) (neighbors graphTest1 v))
                   }

test2 :: Test
test2 = do
  let name = "Test bfs on TestGraph1"
      out = level $ bfs graphTest1 1
      expe = IM.fromList [(1,0),(2,1),(5,1),(6,1),(3,2),(4,2),(7,2)]
   in case  out == expe of
        True -> testPassed name "passed!"
        False -> testFailed name $ (,) (show expe) (show out)

{-test3 :: Test-}
{-test3 = do-}
  {-let name = "Test bfs and adjBFS should give the same results"-}
      {-g = TestGraph1-}
      {-out = level $ bfs graphTest1 1-}
      {-expe = adjBFS g (adjacencyMap g) 1-}
   {-in case  out == expe of-}
        {-True -> testPassed name "passed!"-}
        {-False -> testFailed name $ (,) (show expe) (show out)-}
