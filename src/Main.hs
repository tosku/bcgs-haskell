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

  
{-fg = TestGraph1-}
{-cs = M.fromList $ zip (edges fg) (repeat 1.0)-}
{-out = pushRelabel fg cs 1 7-}

main :: IO ()
main = do
  let name = "weights of RBBC"
      l    = 6
      d    = 2
      latt = graphCubicPBC $ PBCSquareLattice l d
      rbbc = RandomBond { bondDisorder = Unimodal 901 0.3
                        , crystalField = 1.8
                        }
      real = realization'RBBC rbbc latt
      ou = maxFlow fg
      fg' = network'RBBC real
      out = maxFlow fg'

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
  putStrLn $ show $ (fromRational (preflow out) :: Double)
  {-putStrLn $ show $ netNeighbors out 1 -}
  {-putStrLn $ show $ netNeighbors out 2 -}
  {-putStrLn $ show $ netNeighbors out 5 -}

