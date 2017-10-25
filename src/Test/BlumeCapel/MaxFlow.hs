{-# LANGUAGE TemplateHaskell #-}
module Test.BlumeCapel.MaxFlow where

import Language.Haskell.TH
import Control.Lens
import Data.Maybe
import Data.Lattice
import Data.List
import Data.List.Unique
import Test.Test
import qualified Data.Vector as V
import Data.Maybe

import Data.Grid
import Data.BlumeCapel
import Data.BlumeCapel.MaxFlow

fastTests :: [Test]
fastTests = [ test1
            , test2
            , test3
            , test4
            , test5
            ]

test1 :: Test
test1 = do
  let name = "weights of RBBC"
      l    = 20
      d    = 3
      dis  = UnimodalDisorder 901 0.3
      latt = PBCSquareLattice l d
      delta = 1.8
      real = RBBC dis latt delta
      expe = -9561.078976568637
      out = V.sum $ weights real :: Double
  case  out == expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show expe) (show out)

mwis :: (Disorder d, Lattice l) => RBBC d l -> V.Vector Double
mwis r = let js = interactions r
             wi v = 
              let j e = js V.! (fromJust (mapEdgeIndx r e) - 1)
                    in (crystalField r) - (sum $ map j (forwardEdges r v))
        in V.fromList $ map wi (vertices r)

test2 :: Test
test2 = do
  let name = "mapEdgeIndx should equal EdgeIx -- Lattice test"
      l    = 20
      d    = 3
      dis  = UnimodalDisorder 901 0.3
      latt = PBCSquareLattice l d
      delta = 1.8
      real = RBBC dis latt delta
      fws = weights real :: V.Vector Double
      mws = mwis real :: V.Vector Double
  case  fws == mws of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show fws) (show mws)

test3 :: Test
test3 = do
  let name = "flowgraph's edges should be 4*N"
      l    = 20
      d    = 3
      dis  = UnimodalDisorder 1901 0.3
      latt = PBCSquareLattice l d
      delta = 1.8
      real = RBBC dis latt delta
      fg = GSFG real
      out = edges fg
      expe = (size real) * 4
  case length out == fromIntegral expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show expe) (show $ length out)


test4 :: Test
test4 = do
  let name = "capacities should equal abs weights"
      l    = 10
      d    = 3
      dis  = UnimodalDisorder 901 0.3
      latt = PBCSquareLattice l d
      n = fromIntegral $ size latt 
      delta = 1.8
      real = RBBC dis latt delta
      fg = GSFG real
      out = take n $ capacities fg 
      expe = map (\w -> case w < 0 of
                          True -> -w
                          False -> 0.0 
                 ) $ V.toList $ weights real
  case out == expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show $ take 10 (capacities fg)) (show $ V.take 10 (weights real))

test5 :: Test
test5 = do
  let name = "Test Max Flow"
      l    = 7
      d    = 3
      dis  = UnimodalDisorder 901 0.3
      latt = PBCSquareLattice l d
      delta = 2.5
      real = RBBC dis latt delta
      fg = GSFG real
      out = maxFlow fg
      expe = show 3.2
  case out == expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show expe) (show out)

