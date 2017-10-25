{-|
Module      : BFS
Description : The reduction of finding the ground state Blume-Capel realization to the max-flow of a flow graph 
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}
 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE FunctionalDependencies #-}
 {-# LANGUAGE FlexibleInstances #-}


module Data.Graph.BFS
  ( bfs
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF

import Data.Graph

data BFS = BFS { frontier :: S.Set Vertex
               , level :: IM.IntMap Int -- ^ Keeps level of vertex
               , parent :: IM.IntMap Vertex -- ^ Gives parent of vertex
               , maxLevel :: Int
               } deriving (Eq, Show)

skeletonBFS :: Graph g => g -> Vertex -> BFS
skeletonBFS g s = BFS { frontier = S.singleton s
                      , level = IM.fromList [(s,0)]
                      , parent= IM.empty
                      , maxLevel = 0
                      }

bfs :: Graph g => g -> Vertex -> BFS
bfs g s = breadthFirstSearch sbfs
  where sbfs = skeletonBFS g s
        breadthFirstSearch b
          | S.empty == frontier b = b
          | otherwise = bbfs
            where oldLevel = maxLevel b
                  newLevel = oldLevel + 1
                  oldLevels = level b
                  oldFrontiers = frontier b
                  newFrontiers = let toCheck = foldl' (\ac v -> S.union ac (S.fromList (neighbors g v))) S.empty oldFrontiers
                                  in S.filter (\n -> not $ IM.member n oldLevels) toCheck
                  newLevels = foldl' (\ac v -> IM.insert v newLevel ac) oldLevels newFrontiers
                  oldParents = parent b
                  newParents = let toCheck = S.foldl' (\ac v -> S.union ac (S.fromList 
                                             (zip (neighbors g v) (repeat v)) ) ) S.empty oldFrontiers
                                 in S.foldl' (\ac (n,p) -> IM.insert n p ac) oldParents toCheck
                  bbfs = breadthFirstSearch (b { frontier = newFrontiers
                             , level = newLevels 
                             , parent = newParents
                             , maxLevel = newLevel
                             })

