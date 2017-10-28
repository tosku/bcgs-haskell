{-# LANGUAGE TemplateHaskell #-}
module Test.Graph.MaxFlow where

import Language.Haskell.TH
import Control.Lens
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
import qualified Data.Graph.Lattice as Lat
import Data.Graph.BFS
import Data.Graph.Grid

fastTests :: [Test]
fastTests = [ test1
            ]

data TestGraph1 = TestGraph1

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

instance Lat.Lattice TestGraph1 where
  size g = 7
  numEdges g = 11
  edgeIx = Lat.mapEdgeIndx


test1 :: Test
test1 = do
  let name = "Graph.pushrelabel with FGL's MaxFlow"
      g = TestGraph1
      out = level $ bfs g 1
      expe = IM.fromList [(1,0),(2,1),(5,1),(6,1),(3,2),(4,2),(7,2)]
   in case  out == expe of
        True -> testPassed name "passed!"
        False -> testFailed name $ (,) (show expe) (show out)
