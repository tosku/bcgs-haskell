module Main where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntSet as Set
import qualified Data.IntMap.Strict as IM

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import Data.Graph
import Data.Graph.Grid
import Data.BlumeCapel
import Data.BlumeCapel.GSNetwork
import Data.Graph.PushRelabel.Pure
{-import qualified Data.Graph.PushRelabel.STM as IOPR-}


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
  let l    = 60
  let d    = 2
  let latt = graphCubicPBC $ PBCSquareLattice l d
  let rbbc = RandomBond { bondDisorder = Dichotomous 91 0.95
  {-let rbbc = RandomBond { bondDisorder = Unimodal 901 1.15-}
                        , crystalField = 1.987
                        }
  let real = realization'RBBC rbbc latt
  {-let ou = maxFlow tfg-}
  let fg = network'RBBC real
  let vs = map (\v -> (v,())) $ vertices (graph fg) :: [G.UNode]
  let es = map (\(e, c) -> (from e,to e,fromRational c)) $ (M.toList $ capacities fg) :: [G.LEdge Double]
  let mfg = G.mkGraph vs es :: I.Gr () Double
  let expe = MF.maxFlow mfg 0 (sink fg) :: Double
  putStrLn "Started 🏃 \n"

  {-putStrLn "FGL flow"-}
  {-putStrLn $ show expe-}
  {-putStrLn "\n"-}

  putStrLn "Pure pushRelabel"
  eout <- pushRelabel fg
  out <- case eout of 
           Left err -> print err 
           Right gmf -> do
             {-print $ show $ netEdges gmf-}
             {-print $ show $ netVertices gmf-}
             let ovfs = overflowing gmf
             let ovfs' = Set.unions (map snd (IM.toList ovfs))
             putStrLn "number of forward overflowing"
             print $ show $ Set.size ovfs'
             putStrLn "steps"
             print $ show $ steps gmf
             putStrLn "max flow"
             print (fromRational $ netFlow gmf :: Double)


  {-putStrLn "\n"-}
  {-putStrLn "STM pushRelabel flow"-}
  {-eiout <- IOPR.pushRelabel fg-}
  {-case eiout of -}
      {-Left err -> print err-}
      {-Right gmf -> return gmf >> return ()-}

  putStrLn "\n"
  putStrLn "The End!"
