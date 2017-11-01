module Test.BlumeCapel where

import qualified Data.Vector  as V
import qualified Data.Map.Lazy as M
import Data.List
import Data.Either.Unwrap
import Data.Maybe

import Test.Test

import Data.Graph.Grid
import Data.BlumeCapel

fastTests :: [Test]
fastTests = [ test1
            , test2
            , test4
            , test5
            ]

sumJs js = sum $ map snd (M.toList js)

test1 :: Test
test1 = do
  let name = "Dichotomous (strong-weak) Random Bond Blume Capel - half edges should be strong"
      l    = 10
      d    = 3
      dis  = BimodalDisorder 901 8.1
      latt = PBCSquareLattice l d
      delta = 1.8
      real = RBBC dis latt delta
      expe = 0.5
      nume = fromIntegral (numBonds real)
      js   = interactions real
      out  = fromIntegral (M.size (M.filter (\j -> j >= 1) $ js)) / nume
  case out == expe && (all (\x -> (snd x>=0)) (M.toList js)) of
    True -> testPassed name $ show ( (sumJs js) / nume) ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++ " <J>: " ++ show ((sumJs js) / nume))

test2 :: Test
test2 = do
  let name = "Gaussian disorder Random Bond Blume Capel - Sum of Js should be 1"
      l    = (10 :: L)
      d    = (3 :: D)
      delta = 1.8
      real = RBBC (UnimodalDisorder 191 0.1) (PBCSquareLattice l d) delta
      expe = 1
      nume = fromIntegral (numBonds real)
      js = interactions real
      out = (sumJs js) / nume
  case (abs(out - 1) < 0.001) && (M.null (M.filter (\x -> not ((x>=0) && (x<=2))) js)) of
    True -> testPassed name $ show (sumJs js) ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (sumJs js))

test4 :: Test
test4 = do
  let name = "The Energy of the all Zero Configuration should be zero"
      l    = (10 :: L)
      d    = (3 :: D)
      delta = 1
      latt = (PBCSquareLattice l d)
      real = RBBC (UnimodalDisorder 191 0.1) latt delta
      expe = 0
      nume = fromIntegral (gridNumEdges latt)
      js = interactions real
      n  = fromIntegral $ gridSize latt
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
      l    = (10 :: L)
      d    = (3 :: D)
      delta = 3
      latt = (PBCSquareLattice l d)
      real = RBBC (BimodalDisorder 191 0.1) latt delta
      expe = 0
      nume = fromIntegral (numBonds real)
      js = interactions real
      n  = fromIntegral $ gridSize latt
      conf = BCConfiguration (V.fromList $ replicate n Up)
      out = fromRight (energy real conf)
  case abs(out) < 0.000001 of
    True -> testPassed name $ show (sumconfiguration conf) ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (sumconfiguration conf))
