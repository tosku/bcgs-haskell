{-|
Module      : MaxFlow
Description : Maximum Flow - Min Cut - Push relabel algorithm
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}

module Data.Graph.MaxFlow
  ( MaxFlow
  , pushRelabel
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import Data.Graph
import Data.Graph.Lattice

data MaxFlow = MaxFlow

pushRelabel = 3.0
