{-# LANGUAGE TemplateHaskell #-}
module Test.Graph.BFS where

import Language.Haskell.TH
import Control.Lens
import Data.Maybe
import Data.Lattice
import Data.List
import Data.List.Unique
import Test.Test
import qualified Data.Vector as V
import Data.Maybe

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import Data.Graph
import Data.Graph.BFS
import Data.Grid

fastTests :: [Test]
fastTests = [ test1
            ]

test1 :: Test
test1 = do
  let name = "Check with fgl BFS"
      l    = 10
      d    = 3
      latt = PBCSquareLattice l d
      vs = map (\v -> (v,())) $ vertices latt :: [G.UNode]
      es = map (\(f,t) -> (f,t,1.0)) $ (map toTuple (edges latt)) :: [G.LEdge Double]
      mfg = G.mkGraph vs es :: I.Gr () Double
      expe = show $ IBFS.bfs 1 mfg
      mf = show $ MF.maxFlow mfg 1 1000 
      out = show $ bfs latt 1
   in case  out == expe of
        True -> testPassed name "passed!"
        False -> testFailed name $ (,) (show expe) (show out)
