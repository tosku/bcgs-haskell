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


module Data.BlumeCapel
    ( Graph (..)
    , Realization (..)
    , Disorder (..)
    , UnimodalDisorder (..) -- ^ Gaussian (truncated [0,2]) distribution of random bonds
    , BimodalDisorder (..) -- ^ Dichotomous distribution of random bonds 
    , Delta -- ^ Crysta field strength Double
    , RBBC (..) -- ^ Random Bond Blume Capel
    , crystalField
    , BCSpin (..)
    , Spin (..)
    , Configuration (..)
    , BCConfiguration (..)
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

type Energy = Double
type Mag = Int

class Spin s where
  project :: (Num p) => s -> s -> p

newtype IsingSpin = IsingSpin Bool deriving (Show, Eq, Ord) -- probably more memory efficient
instance Spin IsingSpin where
  project a b 
    | a == b =  1
    | a /= b = -1

data BCSpin = Up 
            | Zero 
            | Down 
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance Spin BCSpin where
  project a b
    | a == Up && b == Up = 1
    | a == Down && b == Down = 1
    | a == Up && b == Down = -1
    | a == Down && b == Up = -1
    | otherwise = 0

class Spin s => Configuration c s | c -> s where
  configuration :: c -> V.Vector s
  numSpins :: c -> Int
  spin :: c -> Vertex -> s
  sumconfiguration :: c -> Mag

data BCConfiguration = BCConfiguration (V.Vector BCSpin)

instance Configuration BCConfiguration BCSpin where
  configuration (BCConfiguration c) = c
  numSpins c = V.length $ configuration c
  spin c v = (configuration c) V.! (v - 1)
  sumconfiguration c = foldl (\acc s -> acc + project Up s) 0 (configuration c)

type Delta = Double

type DisorderStrength = Double -- ^ Should be between [0,1]
type J = Edge -> Energy -- ^ Exchange interaction
type Js = M.Map Edge Energy

isingJ :: J
isingJ _ = 1

class Eq r => Disorder r where
  distribution :: r -> Int -> IM.IntMap Energy

data BimodalDisorder = BimodalDisorder Seed DisorderStrength
  deriving (Eq,Show)

instance Disorder BimodalDisorder where
  distribution (BimodalDisorder s d) n = dichotomousJs n s d

dichotomousJs :: Int -> Seed -> DisorderStrength -> IM.IntMap Energy
dichotomousJs n s p = do
  let p' = case (p < 0) || (p > 1) of
           True -> 1
           False -> p
      jWeak = 1.0 - p'
      jStrong = 2.0 - jWeak
      n' = fromIntegral n 
      js = IM.fromList $ zip [1..n'] (repeat jStrong)
      weakindxs = sample (getRNG s :: MTRNG) (quot n' 2) [1 .. n']
   in foldl (\ac i -> IM.insert i jWeak ac) js weakindxs

data UnimodalDisorder = UnimodalDisorder Seed DisorderStrength
  deriving (Eq,Show)

instance Disorder UnimodalDisorder where
  distribution (UnimodalDisorder s d) n = normalJs n s d

normalJs :: Int -> Seed -> DisorderStrength -> IM.IntMap Energy
normalJs n s p = 
  let n' = fromIntegral n
      rng = getRNG s :: MTRNG
      μ = 1
      σ = p
      f = 0
      t = 2
      js = IM.fromList $ zip [1..] (truncatedNormalSample rng μ σ f t n')
   in js

class Realization r where
  size :: r -> Int
  numBonds :: r -> Int
  interactions :: r -> Js
  energy :: (Configuration c s) => r -> c -> Either String Energy
  magnetization :: (Configuration c s) => r -> c -> Either String Mag

data (Disorder d, Graph l) => RBBC d l = RBBC d l Delta
  deriving (Eq)

instance (Disorder d, Graph l) => Graph (RBBC d l) where 
  vertices (RBBC d l f) = vertices l
  edges (RBBC d l f) = edges l
  neighbors (RBBC d l f) = neighbors l
  outEdges (RBBC d l f) = outEdges l
  edgeIndex (RBBC d l f) = edgeIndex l

crystalField :: (Disorder d, Graph l) => RBBC d l -> Delta
crystalField (RBBC d l f) = f

instance (Disorder d, Graph l) => Realization (RBBC d l) where 
  size (RBBC d l f) = numVertices l
  numBonds (RBBC d l f) = numEdges l
  interactions (RBBC d l f) = let eid e = fromJust $ edgeIndex l e
                                  js = distribution d (numEdges l)
                                  getj e = fromJust $ IM.lookup (eid e) js 
                               in M.fromList $ map (\e -> (e, getj e)) (edges l)
  energy r c = case numSpins c == fromIntegral (size r) of
                 True -> let bondenergy = foldl' (\ac e -> 
                                   let (f,t) = toTuple e 
                                       projc conf = project (spin conf f) (spin conf t)
                                       be = (projc c) * (fromJust $ M.lookup e (interactions r))
                                   in ac - be
                               ) 0 (edges r) 
                             siteenergy = foldl' (\ac v -> ac + (project (spin c v) (spin c v)) * (crystalField r)) 0 (vertices r)
                         in Right $ bondenergy + siteenergy
                 False -> Left "Error: spin configuration not compatible with lattice"
  magnetization r c = case numSpins c == fromIntegral (size r) of
                   True -> Right $ sumconfiguration c
                   False -> Left "Error: spin configuration not compatible with lattice"
