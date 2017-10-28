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
    , Lattice (..)
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

import Data.PRNG
import Data.PRNG.MTRNG
import Data.Graph.Grid

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
normalJs n s p = 
  let n' = fromIntegral n
      rng = getRNG s :: MTRNG
      μ = 1
      σ = p
      f = 0
      t = 2
      js = V.fromList $ truncatedNormalSample rng μ σ f t n'
   in js

class Realization r where
  interactions :: r -> Js
  interaction :: r -> Edge -> Maybe Energy
  energy :: (Configuration c s) => r -> c -> Either String Energy
  magnetization :: (Configuration c s) => r -> c -> Either String Mag

data (Disorder d, Lattice l) => RBBC d l = RBBC d l Delta

instance (Disorder d, Lattice l) => Graph (RBBC d l) where 
  vertices (RBBC d l f) = vertices l
  neighbors (RBBC d l f) = neighbors l
  edges (RBBC d l f) = edges l

instance (Disorder d, Lattice l) => Lattice (RBBC d l) where 
  size (RBBC d l f) = size l
  numEdges (RBBC d l f) = numEdges l
  forwardEdges (RBBC d l f) = forwardEdges l
  edgeIx (RBBC d l f) = edgeIx l
  --edgeIx (RBBC d l f) = mapEdgeIndx l


crystalField ::(Disorder d, Lattice l) => RBBC d l -> Delta
crystalField (RBBC d l f) = f

rbbcInteraction r e = let meidx = edgeIx r e
                          js = interactions r
                       in case meidx of 
                            Just eidx -> Just $ js V.! (eidx - 1)
                            Nothing -> Nothing

rbbcInteractions (RBBC d l f) = distribution d (numEdges l)

                 
instance (Disorder d, Lattice l) => Realization (RBBC d l) where 
  interaction = rbbcInteraction
  interactions = rbbcInteractions
  energy r c = case numSpins c == fromIntegral (size r) of
                 True -> let bondenergy = foldl' (\ac (i,e)-> 
                                   let (f,t) = toTuple e 
                                       projc conf = project (spin conf f) (spin conf t)
                                       be = (projc c) * (interactions r V.! (i - 1))
                                   in ac - be
                               ) 0 $ zip [1 .. (fromIntegral (numEdges r) :: Int)] (edges r) 
                             siteenergy = foldl' (\ac v -> ac + (project (spin c v) (spin c v)) * (crystalField r)) 0 (vertices r)
                         in Right $ bondenergy + siteenergy
                 False -> Left "Error: spin configuration not compatible with lattice"
  magnetization r c = case numSpins c == fromIntegral (size r) of
                   True -> Right $ sumconfiguration c
                   False -> Left "Error: spin configuration not compatible with lattice"
