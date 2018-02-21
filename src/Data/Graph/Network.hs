{-|
Module      : Network
Description : Network data type
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}
 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE FunctionalDependencies #-}
 {-# LANGUAGE FlexibleInstances #-}


module Data.Graph.Network
  ( Network (..)
  , Capacity
  , Capacities
  , Flow
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Lazy as M
import qualified Data.IntSet as Set

import Data.Graph

type Capacity = Rational 
type Capacities = M.Map Edge Capacity 
type Flow = Capacity

data Network = Network { graph :: Graph
                       , source :: Vertex
                       , sink :: Vertex
                       , capacities :: Capacities
                       , flow :: Capacities
                       }
                       deriving (Show,Eq)


