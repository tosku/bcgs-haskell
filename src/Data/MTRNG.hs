{-|
Module      : MTRNG
Description : Class definitions

Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

MT RNG Class definitions
 -}

module Data.MTRNG
    ( Seed
    , randomBools
    , randomDoubles
    , uniqueRandomInts
    , sample
    ) where

import Data.Maybe
import         System.Random.Mersenne.Pure64 as MT
import qualified  Data.Vector as V
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


type Seed = Int
type RNG = Seed -> PureMT

getRNG :: RNG 
getRNG s = pureMT $ fromIntegral s

-- | returns a list of booleans given the length of the list list and the seed
randomBools :: Int -> Seed -> Double -> [Bool]
randomBools l s p        =
  let rndbls 0 _         = []
      rndbls n g         = b:(rndbls (n-1) g')
        where (d,g')     = MT.randomDouble g
              b          = d > p
  in rndbls l (getRNG s)

-- | Sample from list 
sample :: [a] -> Int -> Seed -> [a]
sample c n s = c

-- | range (from,to) number of Ints 
uniqueRandomInts :: Int -> Seed -> [Int]
uniqueRandomInts n s = do
  let ls = IntMap.fromList (zip [1..n] [1..n])
  let randoms = zip [1..n] 
                $ map ((\l -> if l > n then n else l ) . floor) $ map (\r -> ((fromIntegral n * r)) + 1) (randomDoubles n s)
  if (n <= 0) then
    []
  else 
    map snd (IntMap.toList $ intmap ls randoms)
    where intmap ls randoms = foldl (\out (fk,sk) -> let mfv = IntMap.lookup fk out
                                                         msv = IntMap.lookup sk out
                                                         unjust = fromMaybe (-1)
                                                     in IntMap.insert sk (unjust mfv) $ IntMap.insert fk (unjust msv) out
                                    ) ls randoms
        
    -- foldl (l z -> z !! l) ls
    -- let range = to - from + 1
    --     sample 0 _ _ = empty
    --     sample l g oldsample = 
    --       case member newrandom oldsample of 
    --       True -> sample l g' oldsample
    --       False -> insert newrandom (sample (l-1) g' (sample l g oldsample))
    --       where (d, g') = MT.randomDouble g
    --             newrandom = floor (d * (fromIntegral range))
    -- in toList $ sample n (getRNG s) empty

-- | returns a list of Doubles given the length of the list list and the Seed
randomDoubles :: Int -> Seed -> [Double]
randomDoubles l s = 
  let rnddbls 0 _ = []
      rnddbls n g = d:(rnddbls (n-1) g')
        where (d,g') = MT.randomDouble g
  in rnddbls l (getRNG s)

-- ! Marsaglia algorithm (needs two seeds)
normalDistribution :: Int -> Seed -> Seed -> Double -> Double -> [Double]
normalDistribution n s1 s2 μ σ =
  -- let getNormal g = MT.randomDouble g)
  let normals 0 _ = []
      normals n g = d:(normals (n-1) g')
        where (d,g') = MT.randomDouble g
  in normals n (getRNG s1)

-- 
-- static bool hasSpare = false;
-- 
-- static double spare;
-- 
-- 
-- if(hasSpare) {
-- 
--   hasSpare = false;
-- 
--   return mean + stdDev * spare;
-- 
-- }
-- 
-- 
-- hasSpare = true;
-- 
-- static double u, v, s;
-- 
-- do {
-- 
--   u = (rand() / ((double) RAND_MAX)) * 2.0 - 1.0;
-- 
--   v = (rand() / ((double) RAND_MAX)) * 2.0 - 1.0;
-- 
--   s = u * u + v * v;
-- 
-- }
-- 
-- while( (s >= 1.0) || (s == 0.0) );
-- 
-- 
-- s = sqrt(-2.0 * log(s) / s);
-- 
-- spare = v * s;
-- 
-- return mean + stdDev * u * s;e
--
randomDouble = MT.randomDouble
