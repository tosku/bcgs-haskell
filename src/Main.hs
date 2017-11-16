module Main where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

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
  

tfg = Network { graph = graphTest1
             , source = 1
             , sink = 7
             , capacities = M.fromList $ zip (edges (graph tfg)) (map toRational $ repeat 1.0)
             , flow = M.empty
             }


main :: IO ()
main = do
  let name = "weights of RBBC"
  let l    = 20
  let d    = 3
  let latt = graphCubicPBC $ PBCSquareLattice l d
  let rbbc = RandomBond { bondDisorder = Unimodal 901 0.3
                        , crystalField = 2.7
                        }
  let real = realization'RBBC rbbc latt
  let ou = maxFlow tfg
  let fg = network'RBBC real
  let vs = map (\v -> (v,())) $ vertices (graph fg) :: [G.UNode]
  let es = map (\(e, c) -> (from e,to e,fromRational c)) $ (M.toList $ capacities fg) :: [G.LEdge Double]
  let mfg = G.mkGraph vs es :: I.Gr () Double
  let expe = MF.maxFlow mfg 0 (sink fg) :: Double

  out <- maxFlow fg
      --bf = latticeBFS real 1
  --putStrLn "Getting max flow" 
  --putStrLn $ show $ maxFlow fg
  {-putStrLn "Getting bfs" -}
  {-putStrLn $ show $ IM.lookup 24 $ level bf-}
  {-putStrLn "Getting gsfg realization"-}
  {-putStrLn $ show $ gsBCCapacities fg-}
  putStrLn "Getting max flow of test graph"
  {-putStrLn "steps"-}
  {-putStrLn $ show $ steps out-}
  putStrLn "pushRelabel flow"
  putStrLn $ show $ (fromRational out :: Double)
  putStrLn "FGL flow"
  {-putStrLn $ show expe-}
  {-putStrLn $ show $ netNeighbors out 1 -}
  {-putStrLn $ show $ netNeighbors out 2 -}
  {-putStrLn $ show $ netNeighbors out 5 -}

