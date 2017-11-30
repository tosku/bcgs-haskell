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
import Data.Graph.PushRelabel.STM
import Data.Graph.BFS
import Data.BlumeCapel
import Data.BlumeCapel.GSNetwork

import Data.PRNG
import Data.PRNG.MTRNG
import Data.Graph

fastTests :: [Test]
fastTests = [ test1
            , test2
            {-, test3-}
            {-, test4-}
            {-, test5-}
            ]


getGS :: L -> D -> Int -> GroundState
getGS l d s =
  let latt = graphCubicPBC $ PBCSquareLattice l d
      {-rbbc = RandomBond { bondDisorder = Dichotomous s 0.95-}
      rbbc = RandomBond { bondDisorder = Unimodal s 1.15
                        , crystalField = 1.987
                        }
      real = realization'RBBC rbbc latt
   in groundState real

testGS :: GroundState -> Bool
testGS gs = cutEnergy gs == (energy $ replica gs)

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

test2 :: Test
test2 = do
  let name = "Cut Energy equals realizations Hamiltonian 100 realizations  L30 d2"
      rng = getRNG 23 :: MTRNG
      seeds = map (floor . ((*) 10000)) $ uniformSample rng 100
      gss = map (getGS 30 2) seeds
      out = filter (not . testGS) gss
  case null out of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) ("error:") (show out)

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

{-test5 :: Test-}
{-test5 = do-}
  {-let name = "Test Max Flow"-}
      {-l    = 8 :: L-}
      {-d    = 2 :: D-}
      {-latt = graphCubicPBC $ PBCSquareLattice l d-}
      {-rbbc = RandomBond { bondDisorder = Unimodal 901 0.3-}
                        {-, crystalField = 1.5-}
                        {-}-}
      {-real = realization'RBBC rbbc latt-}
      {-fg = network'RBBC real-}
      {-mf = maxFlow fg-}
      {-out = fromRational $ netFlow mf-}
      {-expe = 3.2-}
  {-case out == expe of-}
    {-True -> testPassed name "passed!"-}
    {-False -> testFailed name $ (,) (show expe) (show $ steps mf)-}

  {-let fg = network'RBBC real-}
  {-let vs = map (\v -> (v,())) $ vertices (graph fg) :: [G.UNode]-}
  {-let es = map (\(e, c) -> (from e,to e,fromRational c)) $ (M.toList $ capacities fg) :: [G.LEdge Double]-}
  {-let mfg = G.mkGraph vs es :: I.Gr () Double-}
  {-let expe = MF.maxFlow mfg 0 (sink fg) :: Double-}
  {-putStrLn "FGL flow"-}
  {-putStrLn $ show expe-}
  {-putStrLn "\n"-}

  {-putStrLn "Pure pushRelabel"-}
  {-let eout = pushRelabel fg-}
  {-let mfvalue = (fromRational $ netFlow (fromRight eout) :: Double)-}
  {-let sourceCap = (fromRational $ sourceEdgesCapacity fg :: Double)-}
  {-out <- case eout of -}
           {-Left err -> print err -}
           {-Right gmf -> do-}
             {-{-print $ show $ netEdges gmf-}-}
             {-{-print $ show $ netVertices gmf-}-}
             {-let ovfs = overflowing gmf-}
             {-let ovfs' = Set.unions (map snd (IM.toList ovfs))-}
             {-putStrLn "number of forward overflowing"-}
             {-print $ show $ Set.size ovfs'-}
             {-putStrLn "steps"-}
             {-print $ show $ steps gmf-}
             {-putStrLn "max flow"-}
             {-print mfvalue-}
             {-putStrLn "Cut Energy"-}
             {-print $ show (mfvalue - sourceCap)-}

  {-putStrLn "\n"-}
  {-putStrLn "STM pushRelabel flow"-}
  {-eiout <- IOPR.pushRelabel fg-}
  {-case eiout of -}
      {-Left err -> print err-}
      {-Right gmf -> return gmf >> return ()-}


