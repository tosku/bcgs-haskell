module Main where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM

import Data.Graph
import Data.Graph.Grid
import Data.BlumeCapel
import Data.BlumeCapel.GSNetwork
import Data.Graph.MaxFlow


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
  neighbors g 6 = [7,5]
  neighbors g 7 = [4]
  edges = edgesFromNeighbors
  edgeIndex = mapEdgeIndx
  {-outEdges = neighbors -}
  
{-fg = TestGraph1-}
{-cs = M.fromList $ zip (edges fg) (repeat 1.0)-}
{-out = pushRelabel fg cs 1 7-}

main :: IO ()
main = do
  let name = "weights of RBBC"
      l    = 6
      d    = 2
      dis  = UnimodalDisorder 901 0.3
      latt = PBCSquareLattice l d
      delta = 1.8
      real = RBBC dis latt delta
      fg = GSFG real
      out = maxFlow fg

      {-cs = map toRational $ replicate (fromIntegral $ numEdges fg) 1.0-}
      {-out = pushRelabel real (M.map toRational $ interactions real) 1 17-}

      --bf = latticeBFS real 1
  --putStrLn "Getting max flow" 
  --putStrLn $ show $ maxFlow fg
  {-putStrLn "Getting bfs" -}
  {-putStrLn $ show $ IM.lookup 24 $ level bf-}
  {-putStrLn "Getting gsfg realization"-}
  {-putStrLn $ show $ gsBCCapacities fg-}
  putStrLn "Getting max flow of test graph"
  putStrLn $ show out 
  putStrLn $ show $ steps out
  putStrLn $ show $ (fromRational (flow out) :: Double)
  {-putStrLn $ show $ netNeighbors out 1 -}
  {-putStrLn $ show $ netNeighbors out 2 -}
  {-putStrLn $ show $ netNeighbors out 5 -}

