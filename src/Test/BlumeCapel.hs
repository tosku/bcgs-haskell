module Test.BlumeCapel where

import qualified Data.Vector  as V
import Data.List
import Data.Either.Unwrap
import Data.Maybe

import Test.Test

import Data.Graph.Lattice
import Data.Graph.Grid
import Data.BlumeCapel

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
      delta = 1.8
      real = RBBC dis latt delta
      expe = 0.5
      nume = fromIntegral (numEdges real)
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
      delta = 1.8
      real = RBBC (UnimodalDisorder 191 0.1) (PBCSquareLattice l d) delta
      expe = 1
      nume = fromIntegral (numEdges real)
      js = interactions real
      out = (V.sum js) / nume
  case (abs(out - 1) < 0.0001) && (V.all (\x -> (x>=0) && (x<=2)) js) of
    True -> testPassed name $ show (V.sum js) ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (V.sum js))

test3 :: Test
test3 = do
  let name = "read Js from realization"
      l    = (5 :: L)
      d    = (3 :: D)
      lat  = (PBCSquareLattice l d)
      dis = (UnimodalDisorder 191 0.1)
      delta = 1.8
      real = RBBC dis lat delta
      nume = fromIntegral (numEdges real)
      js = interactions real
      out = map (fromJust . (interaction real)) (edges lat)
  case out == V.toList js of
    True -> testPassed name $ show (V.sum js) ++ "passed!"
    False -> testFailed name $ (,) (show (sum out)) (show (V.sum js))

test4 :: Test
test4 = do
  let name = "The Energy of the all Zero Configuration should be zero"
      l    = (50 :: L)
      d    = (3 :: D)
      delta = 1
      real = RBBC (UnimodalDisorder 191 0.1) (PBCSquareLattice l d) delta
      expe = 0
      nume = fromIntegral (numEdges real)
      js = interactions real
      n  = fromIntegral (size real)
      conf = BCConfiguration (V.fromList $ replicate n Zero)
      out = energy real conf
  case out of 
    Right en -> 
      case abs(en) < 0.000001 of
        True -> testPassed name $ show (sumconfiguration conf) ++ "passed!"
        False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (sumconfiguration conf))
    Left err -> testFailed name $ (,) (show expe) (show err)

test5 :: Test
test5 = do
  let name = "The Energy of the Bimodal 3D for Δ=3 should be 0 for the all Up Configuration"
      l    = (50 :: L)
      d    = (3 :: D)
      delta = 3
      real = RBBC (BimodalDisorder 191 0.1) (PBCSquareLattice l d) delta
      expe = 0
      nume = fromIntegral (numEdges real)
      js = interactions real
      n  = fromIntegral (size real)
      conf = BCConfiguration (V.fromList $ replicate n Up)
      out = fromRight (energy real conf)
  case abs(out) < 0.000001 of
    True -> testPassed name $ show (sumconfiguration conf) ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (sumconfiguration conf))
