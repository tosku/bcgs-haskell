module Main where

import Criterion.Main
import qualified Data.Vector as V

import qualified Data.PRNG as RNG
import qualified Data.PRNG.MTRNG as MT

import Data.Grid
import Data.Lattice
import Data.BlumeCapel
import Data.BlumeCapel.MaxFlow

main = do
  let l  = 30
      l2 = 60
      d = 3
      s = 134
      r = 1.9
      delta = 1.7
      ureal = RBBC (UnimodalDisorder s r) (PBCSquareLattice l d) delta
      ureal1 = ureal
      breal = RBBC (BimodalDisorder s r) (PBCSquareLattice l d) delta
      ureal2 = RBBC (UnimodalDisorder s r) (PBCSquareLattice l2 d) delta
      nume = fromIntegral $ numEdges ureal
  writeFile "bench/bench.out" $ show (V.sum (interactions $ ureal) / nume) ++ "\n" ++ 
    show (V.sum $ wis ureal) ++ "\n" ++
    show (V.sum $ wis breal) ++ "\n"
  --print $ show (V.take 9 (interactions ureal)) ++ "\n"
  defaultMain [ 
                bgroup "Blume-Capel realization" [ 
                                                   bench "30^3 uni Weights" $ nf wis ureal1
                                                 , bench "30^3 uni Weights" $ nf mwis ureal1
                                                 , bench "60^3 uni Weights" $ nf wis ureal2
                                                 , bench "60^3 uni Weights" $ nf mwis ureal2
                                                  --bench "20^3 dichotomous" $ nf (\s -> V.sum (breal (20 :: L) (3 :: D) s r) ) seed
                                                 {-{-, bench "20^3 normal" $ nf (\s -> V.sum (ureal (20 :: L) (3 :: D) s r) ) seed-}-}
                                                 {-{-, bench "60^3 dichotomous" $ nf (\s -> V.sum (breal (60 :: L) (3 :: D) s) ) s-}-}
                                                 {-{-, bench "60^3 normal" $ nf (\s -> V.sum (ureal (60 :: L) (3 :: D) s) ) s-}-}
                    ]
              ]
