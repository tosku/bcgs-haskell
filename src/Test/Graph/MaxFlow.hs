{-# LANGUAGE TemplateHaskell #-}
module Test.Graph.MaxFlow where

import Language.Haskell.TH
import Data.Maybe
import Data.List
import Data.List.Unique
import Test.BCTest
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Maybe

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import Data.Graph
import Data.Graph.BFS
import Data.Graph.MaxFlow
import Data.Graph.Grid

fastTests :: [Test]
fastTests = [ 
{-test1-}
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
  

fg = Network { graph = graphTest1
             , source = 1
             , sink = 7
             , capacities = M.fromList $ zip (edges (graph fg)) (map toRational $ repeat 1.0)
             , flow = M.empty
             }

{-test1 :: Test-}
{-test1 = do-}
  {-let name = "Graph.pushRelabel with FGL's MaxFlow"-}
  {-out <- pushRelabel fg-}
  {-let vs = map (\v -> (v,())) $ vertices (graph fg) :: [G.UNode]-}
  {-let es = map (\(f,t) -> (f,t,1.0)) $ (map toTuple (edges (graph fg))) :: [G.LEdge Double]-}
  {-let mfg = G.mkGraph vs es :: I.Gr () Double-}
  {-let expe = MF.maxFlow mfg 1 7 :: Double-}
  {-case out == toRational expe of-}
    {-True -> testPassed name "passed!"-}
    {-False -> testFailed name $ (,) (show expe) (show out)-}
