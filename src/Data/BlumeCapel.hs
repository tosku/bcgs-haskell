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

 -}
 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE FunctionalDependencies #-}
 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE Rank2Types #-}


module Data.BlumeCapel
    ( Graph (..)
    , BondDisorder (..)
    , Delta -- ^ Crysta field strength Double
    , SpinOne (..)
    , Spin (..)
    , RBBC (..) -- ^ Random Bond Blume Capel
    , RandomBond (..)
    , Realization (..)
    , Replica (..)
    , SpinConfiguration (..)
    , BCConfiguration (..)
    , size
    , realization'RBBC
    , replica'RBBC
    ) where

import Data.List
import Data.Maybe
import Data.Either
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import Data.PRNG
import Data.PRNG.MTRNG
import Data.Graph

type Energy = Rational
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

type DisorderStrength = Double -- ^ Should be between [0,1]
type J = Energy -- ^ Exchange interaction strength
type Js = M.Map Edge J
data (Spin s) => Field s = Field (IM.IntMap Energy)
  deriving (Show,Eq)

data BondDisorder = Dichotomous Seed DisorderStrength |
  Unimodal Seed DisorderStrength
  deriving (Show,Eq)

getInteractions :: BondDisorder -> [Edge] -> Js
getInteractions bc es =
  case bc of
    Dichotomous s d -> dichotomousJs es s d
    Unimodal s d -> unimodalJs es s d
    otherwise -> M.empty

dichotomousJs :: [Edge] -> Seed -> DisorderStrength -> Js
dichotomousJs es s p = do
  let p' = case (p < 0) || (p > 1) of
           True -> 1
           False -> p
      jWeak = 1.0 - p'
      jStrong = 2.0 - jWeak
      n = length es
      strongjs = IM.fromList $ zip [1..n] (repeat jStrong)
      weakindxs = sample (getRNG s :: MTRNG) (quot n 2) [1..n]
      js = foldl (\ac i -> IM.insert i jWeak ac) strongjs weakindxs
   in M.fromList $ zip es (map (toRational . snd) $ IM.toList js)

unimodalJs :: [Edge] -> Seed -> DisorderStrength -> Js
unimodalJs es s p = 
  let n = length es
      rng = getRNG s :: MTRNG
      μ = 1
      σ = p
      f = 0
      t = 2
      js = IM.fromList $ zip [1..] (map toRational (truncatedNormalSample rng μ σ f t n))
   in M.fromList $ zip es (map snd $ IM.toList js)

data Spin s => Realization r s = Realization { lattice :: Graph
                                             , interactions :: Js
                                             , fieldCoupling :: s -> Energy
                                             }
instance Spin s => Eq (Realization r s) where
  (==) r1 r2 = let allUp = SpinConfiguration $ IM.fromList (zip [1..(numVertices (lattice r1))] (repeat referenceSpin))
                in interactions r1 == interactions r2 &&
                    getFieldCoupling r1 allUp == getFieldCoupling r2 allUp

getFieldCoupling :: Spin s => Realization r s -> SpinConfiguration s -> [Energy]
getFieldCoupling r c = map ((fieldCoupling r) . fromJust . (spin c)) (vertices (lattice r))

data Spin s => Replica r s = Replica { realization :: Realization r s
                                     , configuration :: SpinConfiguration s
                                     , energy :: Energy
                                     }
instance (Spin s) => Eq (Replica r s) where
  (==) r1 r2 = realization r1 == realization r2 
    && configuration r1 == configuration r2


data RandomBond = RandomBond { bondDisorder :: BondDisorder
                             , crystalField :: Delta
                             }
  deriving (Eq,Show)

type RBBC = Realization RandomBond SpinOne

size :: Spin s => Realization r s -> Int
size r = numVertices $ lattice r

realization'RBBC :: RandomBond -> Graph -> Realization RandomBond SpinOne
realization'RBBC r g = Realization { lattice = g
                                   , interactions = getInteractions (bondDisorder r) (edges g)
                                   , fieldCoupling = (\s -> (project s s) * (crystalField r))
                                   }

replica'RBBC :: Realization RandomBond SpinOne -> (Realization RandomBond SpinOne -> BCConfiguration) -> Replica RandomBond SpinOne
replica'RBBC r rtoc = Replica { configuration = (rtoc r)
                              , energy = let c = (rtoc r)
                                             bondenergy = foldl' (\ac e -> 
                                               let (f,t) = toTuple e 
                                                   projc conf =  project <$> (spin conf f) <*> (spin conf t)
                                                   be = (fromJust $ projc c) * (fromJust $ M.lookup e (interactions r))
                                                in ac - be
                                               ) 0 (edges (lattice r)) 
                                             siteenergy = sum $ getFieldCoupling r c
                                         in bondenergy + siteenergy
                               }


