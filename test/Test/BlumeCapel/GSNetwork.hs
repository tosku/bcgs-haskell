{-|
Module      : Test.BlumeCapel.GSNetwork
Description : Tests for GSNetork
Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX
|-}

{-# LANGUAGE BangPatterns #-}

module Test.BlumeCapel.GSNetwork where

import TestHS

import Data.Maybe
import Data.List
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Data.BlumeCapel
import Data.BlumeCapel.GSNetwork

import Data.Graph.AdjacencyList.Grid
import Data.Graph.AdjacencyList.BFS

import Data.PRNG
import Data.PRNG.MTRNG

fastTests :: [Test]
fastTests = [ test1
            , test2
            ]


getGS :: L -> D -> Int -> GroundState
getGS l d s =
  let latt = graphCubicPBC $ PBCSquareLattice l d
      {-rbbc = RandomBond { bondDisorder = Dichotomous s 0.95-}
      rbbc = RandomBond { bondDisorder = Unimodal s 1.15 1.987
                        , crystalField = 1.987
                        }
      real = realization'RBBC rbbc latt
   in groundState real


test1 :: Test
test1 = do
  let name = "weights of RBBC"
      l    = 20 :: L
      d    = 3 :: D
      !latt = graphCubicPBC $ PBCSquareLattice l d
      rbbc = RandomBond { bondDisorder = Unimodal 901 0.3 1.8
                        , crystalField = 1.8
                        }
      real = realization'RBBC rbbc latt
      expe = -9681.193769972098
      out = fromRational $ IM.foldr (\w ac -> ac + w) 0 $ weights real :: Double
  case  out == expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show expe) (show out)

-- | Test of tests! Guarantees max flow algorithm and graph reduction.
-- The energy as calculated by the cut of the reduced graph and compares it
-- whith that of the coresponding ground state spin configuration coupled with
-- the realization
test2 :: Test
test2 = do
  let name = "Integration Test: \n Cut Energy equals realizations Hamiltonian 100 realizations L20 d2"
      rng = getRNG 139 :: MTRNG
      seeds = map (floor . ((*) 10000)) $ uniformSample rng 100
      gss = map (getGS 20 2) seeds
      out = filter (not . testGS) gss
  case null out of
    True -> testPassed name "passed!"
    {-True -> testPassed name ("passed!" ++ show ( map cutEnergy gss) ++ show ( map (energy . replica) gss) )-}
    False -> testFailed name $ (,) ("error:") (show out)
  where
    testGS :: GroundState -> Bool
    testGS gs = cutEnergy gs == (energy $ replica gs)
