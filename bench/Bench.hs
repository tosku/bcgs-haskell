module Main where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM

import qualified Data.PRNG as RNG
import qualified Data.PRNG.MTRNG as MT

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import qualified Data.Lattice as Lat
import Data.Graph.BFS
import Data.Grid
import Data.BlumeCapel
import Data.BlumeCapel.GSNetwork


main = do
  let l  = 20
      l2 = 40
      d = 3
      s = 134
      r = 0.9
      delta = 2.5
      ureal = RBBC (UnimodalDisorder s r) (PBCSquareLattice l d) delta
      ureal2 = RBBC (UnimodalDisorder s r) (PBCSquareLattice l2 d) delta
      breal = RBBC (BimodalDisorder s r) (PBCSquareLattice l d) delta
      nume = fromIntegral $ numEdges ureal
  --writeFile "bench/bench.out" $ show (V.sum (interactions $ ureal) / nume) ++ "\n" ++ 
    --show (V.sum $ weights ureal) ++ "\n" ++
    --show (V.sum $ weights breal) ++ "\n"
      latt = PBCSquareLattice l d
      dis  = UnimodalDisorder s r
      real = RBBC dis latt delta
      fg = GSFG real
      bf = adjBFS fg (adjacencyMap fg) 0
      vs = map (\v -> (v,())) $ vertices fg :: [G.UNode]
      es = map (\(f,t) -> (f,t,1.0)) $ (map toTuple (edges fg)) :: [G.LEdge Double]
      mfg = G.mkGraph vs es :: I.Gr () Double
  writeFile "bench/bench.out" $ show (adjBFS fg (adjacencyMap fg) 0)
  defaultMain [ 
                bgroup "Blume-Capel realization" [ 
                                                   bench "20^3 unimodal weights" $ nf weights ureal
                                                 , bench "40^3 unimodal weights" $ nf weights ureal2
                    ]
              ]
