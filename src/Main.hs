module Main where


import Data.List
import Data.Maybe
import qualified Data.Vector as V

import Data.Graph
import Data.Graph.BFS
import Data.Grid
import Data.BlumeCapel
import Data.BlumeCapel.MaxFlow


main :: IO ()
main = do
  let name = "weights of RBBC"
      l    = 40
      d    = 3
      dis  = UnimodalDisorder 901 0.3
      latt = PBCSquareLattice l d
      delta = 1.8
      real = RBBC dis latt delta
      fg = GSFG real
      bf = show $ bfs latt 1
  --putStrLn "Getting max flow" 
  --putStrLn $ show $ maxFlow fg
  putStrLn "Getting bfs" 
  putStrLn $ show bf

