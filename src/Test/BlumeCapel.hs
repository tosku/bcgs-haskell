module Test.BlumeCapel where

import qualified Data.Vector  as V
import Data.List
import Data.Maybe

import Test.Test

import Data.BlumeCapel
import Data.Grid
import Data.Lattice

fastTests :: [Test]
fastTests = [ test1
            , test2
            , test3
            , test4
            , test5
            ]

test1 :: Test
test1 = do
  let name = "Dichotomous (strong-weak) Random Bond Blume Capel - half edges should be strong"
      l    = 20
      d    = 3
      dis  = BimodalDisorder 901 8.1
      latt = PBCSquareLattice l d
      real = RBBC dis latt
      expe = 0.5
      nume = fromIntegral (numEdges (lattice real))
      js   = interactions real
      out  = fromIntegral (V.length (V.filter (\j -> j >= 1) $ js)) / nume
  case out == expe && (V.all (\x -> (x>=0)) js) of
    True -> testPassed name $ show (V.sum js / nume) ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++ " <J>: " ++ show (V.sum js / nume))

test2 :: Test
test2 = do
  let name = "Gaussian disorder Random Bond Blume Capel - Sum of Js should be 1"
      l    = (100 :: L)
      d    = (3 :: D)
      real = RBBC (UnimodalDisorder 191 0.1) (PBCSquareLattice l d)
      expe = 1
      nume = fromIntegral (numEdges (lattice real))
      js = interactions real
      out = (V.sum js) / nume
  case (abs(out - 1) < 0.0001) && (V.all (\x -> (x>=0) && (x<=2)) js) of
    True -> testPassed name $ show (V.sum js) ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (V.sum js))

test3 :: Test
test3 = do
  let name = "read Js from realization"
      l    = (30 :: L)
      d    = (3 :: D)
      lat  = (PBCSquareLattice l d)
      dis = (UnimodalDisorder 191 0.1)
      real = RBBC dis lat
      nume = fromIntegral (numEdges (lattice real))
      js = interactions real
      out = map (getinteraction dis lat) (edges lat)
  case out == V.toList js of
    True -> testPassed name $ show (V.sum js) ++ "passed!"
    False -> testFailed name $ (,) (show (sum out)) (show (V.sum js))

test4 :: Test
test4 = do
  let name = "The Energy of the all Zero Configuration should be zero"
      l    = (50 :: L)
      d    = (3 :: D)
      delta = 1
      real = RBBC (UnimodalDisorder 191 0.1) (PBCSquareLattice l d)
      expe = 0
      nume = fromIntegral (numEdges (lattice real))
      js = interactions real
      n  = fromIntegral (Data.Grid.size (lattice real))
      conf = BCConfiguration (V.fromList $ replicate n Zero)
      out = fromJust $ energy real delta conf
  case abs(out) < 0.000001 of
    True -> testPassed name $ show (magnetization conf) ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (magnetization conf))

test5 :: Test
test5 = do
  let name = "The Energy of the Bimodal 3D for Δ=3 should be 0 for the all Up Configuration"
      l    = (50 :: L)
      d    = (3 :: D)
      delta = 3
      real = RBBC (BimodalDisorder 191 0.1) (PBCSquareLattice l d)
      expe = 0
      nume = fromIntegral (numEdges (lattice real))
      js = interactions real
      n  = fromIntegral (Data.Grid.size (lattice real))
      conf = BCConfiguration (V.fromList $ replicate n Up)
      out = fromJust $ energy real delta conf
  case abs(out) < 0.000001 of
    True -> testPassed name $ show (magnetization conf) ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (magnetization conf))
