{-# LANGUAGE TemplateHaskell #-}
module Test.BlumeCapel.GSNetwork where

import Language.Haskell.TH
import Data.Maybe
import Data.List
import Data.List.Unique
import Test.Test
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap as IM

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import Data.Graph
import Data.Graph.Grid
import Data.Graph.MaxFlow
import Data.Graph.BFS
import Data.BlumeCapel
import Data.BlumeCapel.GSNetwork

fastTests :: [Test]
fastTests = [ test1
            {-, test3-}
            {-, test4-}
            , test5
            ]

test1 :: Test
test1 = do
  let name = "weights of RBBC"
      l    = 20 :: L
      d    = 3 :: D
      latt = graphCubicPBC $ PBCSquareLattice l d
      rbbc = RandomBond { bondDisorder = Unimodal 901 0.3
                        , crystalField = 1.8
                        }
      real = realization'RBBC rbbc latt
      expe = -9561.078976568599
      out = fromRational $ IM.fold (\ac w -> ac + w) 0 $ weights real :: Double
  case  out == expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show expe) (show out)

{-test3 :: Test-}
{-test3 = do-}
  {-let name = "flowgraph's edges should be 4*N"-}
      {-l    = 20-}
      {-d    = 3-}
      {-dis  = UnimodalDisorder 1901 0.3-}
      {-latt = PBCSquareLattice l d-}
      {-delta = 1.8-}
      {-real = RBBC dis latt delta-}
      {-fg = GSFG real-}
      {-out = edges fg-}
      {-expe = (size real) * 4-}
  {-case length out == fromIntegral expe of-}
    {-True -> testPassed name "passed!"-}
    {-False -> testFailed name $ (,) (show expe) (show $ length out)-}

{-test4 :: Test-}
{-test4 = do-}
  {-let name = "Compare fgl BFS and Graph.BFS"-}
      {-l    = 10-}
      {-d    = 2-}
      {-latt = PBCSquareLattice l d-}
      {-dis  = UnimodalDisorder 91 1.7-}
      {-delta = 2.5-}
      {-real = RBBC dis latt delta-}
      {-fg = GSFG real-}
      {-bf = adjBFS fg (adjacencyMap fg) 0-}
      {-out = level bf-}
      {-vs = map (\v -> (v,())) $ vertices fg :: [G.UNode]-}
      {-es = map (\(f,t) -> (f,t,1.0)) $ (map toTuple (edges fg)) :: [G.LEdge Double]-}
      {-mfg = G.mkGraph vs es :: I.Gr () Double-}
      {-expe = IM.fromList $ IBFS.level 0 mfg-}
   {-in case  out == expe of-}
        {-True -> testPassed name "passed!"-}
        {-False -> testFailed name $ (,) (show expe) (show out)-}

test5 :: Test
test5 = do
  let name = "Test Max Flow"
      l    = 8 :: L
      d    = 2 :: D
      latt = graphCubicPBC $ PBCSquareLattice l d
      rbbc = RandomBond { bondDisorder = Unimodal 901 0.3
                        , crystalField = 1.5
                        }
      real = realization'RBBC rbbc latt
      fg = network'RBBC real
      mf = maxFlow fg
      out = fromRational $ preflow $ mf
      expe = 3.2
  case out == expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show expe) (show $ steps mf)

