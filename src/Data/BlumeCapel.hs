{-|
Module      : BlumeCapel
Description : Blume Capel model's realization and properties definitions
Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

- Realization
- get Ennergy of lattice
- get Magnetization of lattice

|-}

 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE FunctionalDependencies #-}
 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE Rank2Types #-}
 {-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
 {-# LANGUAGE BangPatterns #-}


module Data.BlumeCapel
    ( Graph (..)
    , Edge (..)
    , Vertex (..)
    , numEdges
    , Energy
    , showEnergy
    , from 
    , to
    , toTuple
    , fromTuple
    , BondDisorder (..)
    , DisorderStrength
    , Delta -- ^ Crysta field strength Double
    , SpinOne (..)
    , Spin (..)
    , RBBC (..) -- ^ Random Bond Blume Capel
    , RBBCReplica (..) -- ^ Random Bond Blume Capel
    , RandomBond (..)
    , Realization (..)
    , Replica (..)
    , SpinConfiguration (..)
    , BCConfiguration (..)
    , ZeroDistribution (..)
    , size
    , realization'RBBC
    , replica'RBBC
    , getMagnetization
    , zeroCusterSizes
    ) where

import qualified GHC.Generics as GEN

import Data.List
import Data.Maybe
import Data.Either
import Data.Ratio
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import Data.PRNG
import Data.PRNG.MTRNG
import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.BFS

type Energy = Rational
showEnergy :: Energy -> String
showEnergy e = show (fromRational e :: Double)

type Magnetization = Rational

class Eq s => Spin s where
  referenceSpin :: s -- ^ axis of reference (Up) - needed for measuring magnetization 
  project :: s -> s -> Magnetization

data SpinOne = Up 
            | Zero 
            | Down 
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance Spin SpinOne where
  referenceSpin = Up
  project a b
    | a == Up && b == Up = 1
    | a == Down && b == Down = 1
    | a == Up && b == Down = -1
    | a == Down && b == Up = -1
    | otherwise = 0

newtype Spin s => SpinConfiguration s = SpinConfiguration (IM.IntMap s)
  deriving (Show,Eq)

spin :: Spin s => SpinConfiguration s -> Vertex -> Maybe s
spin (SpinConfiguration c) v = IM.lookup v c

getMagnetization :: Spin s => SpinConfiguration s -> Magnetization
getMagnetization (SpinConfiguration c) = foldl (\ac s -> ac + (project referenceSpin s)) 0 c

type BCConfiguration = SpinConfiguration SpinOne

type Delta = Rational

type DisorderStrength = Rational -- ^ Should be between [0,1]
type J = Energy -- ^ Exchange interaction strength
type Js = M.Map Edge J
data (Spin s) => Field s = Field (IM.IntMap Energy)
  deriving (Show,Eq)

data BondDisorder = Dichotomous Seed DisorderStrength |
  Unimodal Seed DisorderStrength
  deriving (Show,Eq,GEN.Generic)

getInteractions :: BondDisorder -> [Edge] -> Js
getInteractions bc es =
  case bc of
    Dichotomous s d -> dichotomousJs es s d
    Unimodal s d -> unimodalJs es s d

dichotomousJs :: [Edge] -> Seed -> DisorderStrength -> Js
dichotomousJs es s r = do
  let r' = case (r < 0) || (r > 1) of
           True -> 1
           False -> r
      -- | r=1-(jw/js)
      jWeak = 2 * (1 - r') / (2 - r')
      jStrong = 2 / (2 - r')
      n = length es
      strongjs = IM.fromList $ zip [1..n] (repeat jStrong)
      weakindxs = sample (getRNG s :: MTRNG) (quot n 2) [1..n]
      js = foldl (\ac i -> IM.insert i jWeak ac) strongjs weakindxs
   in M.fromList $ zip es (map snd $ IM.toList js)

unimodalJs :: [Edge] -> Seed -> DisorderStrength -> Js
unimodalJs es s r = 
  let n = length es
      newseed = s + (fromIntegral $ numerator r) -- ^ (s,r) pair should define the realization
      rng = getRNG newseed :: MTRNG
      μ = 1
      σ = fromRational r
      f = 0
      t = 2
      !js = IM.fromList $ zip [1..] (map toRational (truncatedNormalSample rng μ σ f t n))
   in M.fromList $ zip es (map snd $ IM.toList js)

data Spin s => Realization r s = Realization { lattice :: !Graph
                                             , interactions :: !Js
                                             , fieldCoupling :: s -> Energy
                                             }
instance Spin s => Show (Realization r s) where
  show r = "lattice: " ++ show (lattice r)
    ++ " interactions: " ++ show (interactions r)
instance Spin s => Eq (Realization r s) where
  (==) r1 r2 = let allUp = SpinConfiguration $ IM.fromList (zip [1..(numVertices (lattice r1))] (repeat referenceSpin))
                in interactions r1 == interactions r2 &&
                    getFieldCoupling r1 allUp == getFieldCoupling r2 allUp

getFieldCoupling :: Spin s => Realization r s -> SpinConfiguration s -> [Energy]
getFieldCoupling r c = map ((fieldCoupling r) . fromJust . (spin c)) (vertices (lattice r))

data Spin s => Replica r s = Replica { realization :: !(Realization r s)
                                     , configuration :: !(SpinConfiguration s)
                                     , energy :: Energy
                                     }
instance (Spin s) => Show (Replica r s) where
  show repl = "replica vertices:" ++ show (numVertices $ lattice $ realization repl)
    ++ "\n magnetization: " ++ show ((fromRational $ getMagnetization $ configuration repl) / (fromIntegral $ numVertices $ lattice $ realization repl) :: Double)
    ++ "\n energy: " ++ show (fromRational (energy repl)::Double)
instance (Spin s) => Eq (Replica r s) where
  (==) r1 r2 = realization r1 == realization r2 
    && configuration r1 == configuration r2


data RandomBond = RandomBond { bondDisorder :: BondDisorder
                             , crystalField :: Delta
                             }
  deriving (Eq,Show)

type RBBC = Realization RandomBond SpinOne

type RBBCReplica = Replica RandomBond SpinOne

size :: Spin s => Realization r s -> Int
size r = numVertices $ lattice r

realization'RBBC :: RandomBond -> Graph -> RBBC
realization'RBBC r g = Realization { lattice = g
                                   , interactions = getInteractions (bondDisorder r) (edges g)
                                   , fieldCoupling = (\s -> (project s s) * (crystalField r))
                                   }

replica'RBBC :: Realization RandomBond SpinOne 
             -> BCConfiguration 
             -> RBBCReplica
replica'RBBC r conf = Replica { realization = r
                              , configuration = conf
                              , energy = let bondenergy = foldl' (\ac e -> 
                                               let (f,t) = toTuple e 
                                                   projc conf =  project <$> (spin conf f) <*> (spin conf t)
                                                   be = (fromJust $ projc conf) * (fromJust $ M.lookup e (interactions r))
                                                in ac - be
                                               ) 0 (edges (lattice r)) 
                                             siteenergy = sum $ getFieldCoupling r conf
                                          in bondenergy + siteenergy
                               }

-- | Maps size to frequency of zero clusters
type ZeroDistribution = IM.IntMap Int

zeroCusterSizes :: RBBCReplica -> ZeroDistribution
zeroCusterSizes r =
  let conf = configuration r
      lat = makeUndirected $
             filterVertices 
               (\v -> fromJust (spin conf v) == Zero)
               $ lattice $ realization r
      nvs = vertices lat
      countCluster g dist =
        let vs = vertices g
         in case null vs of
              True -> dist
              False ->
                let v = head vs
                    cluster = bfs g v 
                    zvs = level cluster
                    clustersize = IM.size zvs
                    dist' = case IM.lookup clustersize dist of
                              Just cs -> 
                                IM.adjust (\cs -> cs + 1) cs dist
                              Nothing -> 
                                IM.insert clustersize 1 dist
                    g' = filterVertices 
                         (\v -> case IM.lookup v zvs of
                                  Just _ -> False
                                  Nothing -> True)
                         g
                 in countCluster g' dist'

   in case null nvs of
        True
          -> IM.empty
        False 
          -> countCluster lat IM.empty

