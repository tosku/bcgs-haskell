{-# LANGUAGE TemplateHaskell #-}
module Test.Graph.MaxFlow where

import Language.Haskell.TH
import Data.Maybe
import Data.List
import Data.List.Unique
import Test.Test
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
fastTests = [ test1
            ]

data TestGraph1 = TestGraph1

instance Eq TestGraph1 where
  (==) _ _ = True

instance Graph TestGraph1 where
  vertices g = [1..7]
  neighbors g 1 = [2,5,6]
  neighbors g 2 = [5,3]
  neighbors g 3 = [4]
  neighbors g 4 = []
  neighbors g 5 = [4,7]
  neighbors g 6 = [7]
  neighbors g 7 = [4]
  edges = edgesFromNeighbors
  edgeIndex = mapEdgeIndx
  outEdges g v = map (\n -> Edge v n) $ filter (\n -> v < n) (neighbors g v)
  

fg = TestGraph1
{-cs = M.fromList $ zip (edges fg) (repeat 1.0)-}
cs = M.fromList $ zip (edges fg) (map toRational $ repeat 1.0)

test1 :: Test
test1 = do
  let name = "Graph.pushRelabel with FGL's MaxFlow"
      out = pushRelabel fg cs 1 7
      vs = map (\v -> (v,())) $ vertices fg :: [G.UNode]
      es = map (\(f,t) -> (f,t,1.0)) $ (map toTuple (edges fg)) :: [G.LEdge Double]
      mfg = G.mkGraph vs es :: I.Gr () Double
      expe = MF.maxFlow mfg 1 7 :: Double
   in case  flow out == toRational expe of
        True -> testPassed name "passed!"
        False -> testFailed name $ (,) (show expe) (show $ flow out)
