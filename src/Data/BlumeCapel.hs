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
    ( Realization (..)
    , UnimodalDisorder (..) -- ^ Gaussian (truncated [0,2]) distribution of random bonds
    , BimodalDisorder (..) -- ^ Dichotomous distribution of random bonds 
    , RBBC (..) -- ^ Random Bond Blume Capel
    , lattice
    , interactions
    , energy
    , BCSpin (..)
    , Spin (..)
    , Configuration (..)
    , BCConfiguration (..)
    ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V

import Data.PRNG
import Data.PRNG.MTRNG
import Data.Grid

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
  size :: c -> Int
  spin :: c -> Vertex -> s
  magnetization :: c -> Mag

data BCConfiguration = BCConfiguration (V.Vector BCSpin)

instance Configuration BCConfiguration BCSpin where
  configuration (BCConfiguration c) = c
  size c = V.length $ configuration c
  spin c v = (configuration c) V.! (v - 1)
  magnetization c = foldl (\acc s -> acc + project Up s) 0 (configuration c)

type Delta = Double

type DisorderStrength = Double -- ^ Should be between [0,1]
type J = Edge -> Energy -- ^ Exchange interaction
type Js = V.Vector Energy

isingJ :: J
isingJ _ = 1

class Disorder r where
  distribution :: r -> Natural -> Js

data BimodalDisorder = BimodalDisorder Seed DisorderStrength

instance Disorder BimodalDisorder where
  distribution (BimodalDisorder s d) n = dichotomousJs n s d

dichotomousJs :: Natural -> Seed -> DisorderStrength -> Js
dichotomousJs n s p = do
  let p' = case (p < 0) || (p > 1) of
           True -> 1
           False -> p
  let jWeak = 1.0 - p'
  let jStrong = 2.0 - jWeak
  let n' = fromIntegral n 
  let js = V.replicate n' jStrong
  let weakindxs = zip (sample (getRNG s :: MTRNG) (quot n' 2) [0 .. n'-1] ) (replicate (quot n' 2) jWeak)
  js V.// weakindxs

data UnimodalDisorder = UnimodalDisorder Seed DisorderStrength

instance Disorder UnimodalDisorder where
  distribution (UnimodalDisorder s d) n = normalJs n s d

normalJs :: Natural -> Seed -> DisorderStrength -> Js
normalJs n s p = do
  let n' = fromIntegral n
  let rng = getRNG s :: MTRNG
  let μ = 1
  let σ = p
  let f = 0
  let t = 2
  let js = V.fromList $ truncatedNormalSample rng μ σ f t n'
  js

class (Disorder d, Lattice l) => Realization d l where
  getlattice :: d -> l -> l
  getinteractions :: d -> l -> Js
  getinteraction :: d -> l -> J
  hamiltonian :: d -> l -> Delta -> BCConfiguration -> Maybe Energy

instance (Disorder d, Lattice l) => Realization d l where 
  getlattice d l = l 
  getinteractions d l = distribution d (numEdges l)
  getinteraction d l e = (getinteractions d l) V.! ((fromJust (edgeIx l e)) - 1)
  hamiltonian d l f c = case Data.BlumeCapel.size c == fromIntegral (Data.Grid.size l) of
                          True -> let bondenergy = foldl' (\ac (i,e)-> 
                                                  let (f,t) = toTuple e 
                                                      projc conf = project (spin conf f) (spin conf t)
                                                      be = (projc c) * (getinteractions d l V.! (i - 1))
                                                   in ac - be
                                               ) 0 $ zip [1 .. (fromIntegral (numEdges l) :: Int)] (edges l) 
                                      siteenergy = foldl' (\ac v -> ac + (project (spin c v) (spin c v)) * f) 0 (vertices l)
                                   in pure $ bondenergy + siteenergy
                          False -> Nothing


data (Disorder d, Lattice l) => RBBC d l = RBBC d l
                                                
lattice :: (Disorder d, Lattice l) => RBBC d l -> l
lattice (RBBC d l) = getlattice d l

interactions :: (Disorder d, Lattice l) => RBBC d l -> Js
interactions (RBBC d l) = getinteractions d l

energy :: (Disorder d, Lattice l) => RBBC d l -> Delta -> BCConfiguration -> Maybe Energy
energy (RBBC d l) f c = hamiltonian d l f c
