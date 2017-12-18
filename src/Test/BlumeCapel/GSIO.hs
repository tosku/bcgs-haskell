{-|
Module      : GSIO
Description : Tests for Data.BlumeCapel.GSIO
Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

- Realization
- get Ennergy of lattice
- get Magnetization of lattice

 -}
{-# LANGUAGE TemplateHaskell #-}
module Test.BlumeCapel.GSIO where

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
import qualified Data.BlumeCapel.GSIO as GSIO

import Data.PRNG
import Data.PRNG.MTRNG
import Data.Graph

fastTests :: [Test]
fastTests = [ 
              {-test2-}
            ]

ioTests :: [Test]
ioTests = [ test1
          ]



getGS :: L -> D -> Int -> GroundState
getGS l d s =
  let latt = graphCubicPBC $ PBCSquareLattice l d
      {-rbbc = RandomBond { bondDisorder = Dichotomous s 0.95-}
      rbbc = RandomBond { bondDisorder = Unimodal s 1
                        , crystalField = 1.987
                        }
      real = realization'RBBC rbbc latt
   in groundState real

testGS :: GroundState -> Bool
testGS gs = cutEnergy gs == (GSIO.energy $ replica gs)

test1 :: Test
test1 = do
  let name = "Write and read 20 ground states"
      rng = getRNG 23 :: MTRNG
      seeds = map (floor . ((*) 10000)) $ uniformSample rng 20
      gss = map (getGS 10 2) seeds
      out = filter (not . testGS) gss
  case null out of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) ("error:") (show out)
  {-I.writeFile file (encodeToLazyText recs')-}
