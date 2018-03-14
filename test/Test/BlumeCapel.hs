module Test.BlumeCapel where

import TestHS

import qualified Data.Vector  as V
import qualified Data.Map.Lazy as M
import Data.List
import Data.Either.Unwrap
import Data.Maybe

import Data.BlumeCapel

import Data.Graph.AdjacencyList.Grid

fastTests :: [Test]
fastTests = [ test1
            {-, test2-}
            {-, test4-}
            ]

sumJs js = sum $ map snd (M.toList js)

test1 :: Test
test1 = do
  let name = "Dichotomous (strong-weak) Random Bond Blume Capel - half edges should be strong"
      l    = 10
      d    = 3
      latt = graphCubicPBC $ PBCSquareLattice l d
      rbbc = RandomBond { bondDisorder = Dichotomous 901 8.1 1.8
                        , crystalField = 1.8
                        }
      real = realization'RBBC rbbc latt
      expe = 1500
      nume = (numEdges $ lattice real)
      js   = interactions real
      out  = ((M.size (M.filter (\j -> j >= (toRational 1)) $ js)))
  case out == expe && (all (\x -> (snd x>=0)) (M.toList js)) of
    True -> testPassed name $ show ( (sumJs js)) ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++ " <J>: " ++ show ((sumJs js)))

{-test2 :: Test-}
{-test2 = do-}
  {-let name = "Gaussian disorder Random Bond Blume Capel - Sum of Js should be 1"-}
      {-l    = (10 :: L)-}
      {-d    = (3 :: D)-}
      {-latt = PBCSquareLattice l d-}
      {-rbbc = RBBC { disorder = BimodalDisorder 901 8.1-}
                  {-, crystalField = 1.8-}
                  {-}-}
      {-real = realization'RBBC latt rbbc-}
      {-expe = 1-}
      {-nume = (numEdges $ lattice real)-}
      {-js = interactions real-}
      {-out = (sumJs js) / nume-}
  {-case (abs(out - 1) < 0.001) && (M.null (M.filter (\x -> not ((x>=0) && (x<=2))) js)) of-}
    {-True -> testPassed name $ show (sumJs js) ++ "passed!"-}
    {-False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (sumJs js))-}

{-test4 :: Test-}
{-test4 = do-}
  {-let name = "The Energy of the all Zero Configuration should be zero"-}
      {-l    = (10 :: L)-}
      {-d    = (3 :: D)-}
      {-latt = PBCSquareLattice l d-}
      {-rbbc = RBBC { disorder = BimodalDisorder 901 8.1-}
                  {-, crystalField = 1-}
                  {-}-}
      {-real = realization'RBBC latt rbbc-}
      {-expe = 0-}
      {-nume = gridNumEdges latt-}
      {-js = interactions real-}
      {-n  = fromIntegral $ gridSize latt-}
      {-conf = (\real -> let n = fromIntegral $ length $ vertices (lattice real) -}
                        {-in BCConfiguration (IM.fromList $ zip [1..n] (repeat Zero)))-}
      {-repl = replica'RBBC real conf-}
      {-out = hamiltonian repl-}
  {-case abs(out) < 0.000001 of-}
        {-True -> testPassed name $ show (sumconfiguration conf) ++ "passed!"-}
        {-False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (sumconfiguration conf))-}

{-{-test5 :: Test-}-}
{-{-test5 = do-}-}
  {-{-let name = "The Energy of the Bimodal 3D for Δ=3 should be 0 for the all Up Configuration"-}-}
      {-{-l    = (10 :: L)-}-}
      {-{-d    = (3 :: D)-}-}
      {-{-delta = 3-}-}
      {-{-latt = (PBCSquareLattice l d)-}-}
      {-{-real = RBBC (BimodalDisorder 191 0.1) latt delta-}-}
      {-{-expe = 0-}-}
      {-{-nume = fromIntegral (numBonds real)-}-}
      {-{-js = interactions real-}-}
      {-{-n  = fromIntegral $ gridSize latt-}-}
      {-{-conf = BCConfiguration (V.fromList $ replicate n Up)-}-}
      {-{-out = fromRight (energy real conf)-}-}
  {-{-case abs(out) < 0.000001 of-}-}
    {-{-True -> testPassed name $ show (sumconfiguration conf) ++ "passed!"-}-}
    {-{-False -> testFailed name $ (,) (show expe) ((show out) ++" "++ show (sumconfiguration conf))-}-}
