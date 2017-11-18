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
  , adjBFS
  , BFS (..)
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import Data.Graph

data BFS = BFS { frontier :: S.Set Vertex
               , level :: IM.IntMap Int -- ^ Keeps level of vertex
               , parent :: IM.IntMap Vertex -- ^ Gives parent of vertex
               , maxLevel :: Int
               , topSort :: [Vertex]
               } deriving (Eq, Show)

initialBFS :: Vertex -> BFS
initialBFS s = BFS { frontier = S.singleton s
                      , level = IM.fromList [(s,0)]
                      , parent= IM.empty
                      , maxLevel = 0
                      , topSort = []
                      }

-- | BFS for implicit neighbor definition (grids, infinite graphs)
bfs :: Graph -> Vertex -> BFS
bfs g s = breadthFirstSearch sbfs
  where sbfs = initialBFS s
        breadthFirstSearch b
          | S.empty == frontier b = b
          | otherwise = bbfs
            where oldLevel = maxLevel b
                  newLevel = oldLevel + 1
                  oldLevels = level b
                  oldFrontiers = frontier b
                  frontPar = let toCheck = foldl' (\ac v -> S.union ac (S.fromList (zip (neighbors g v) (repeat v)))) S.empty oldFrontiers
                                in S.filter (\(n, p) -> not $ IM.member n oldLevels) toCheck
                  newFrontiers = S.map fst frontPar
                  oldParents = parent b
                  newParents = S.foldl' (\ac (n,p) -> IM.insert n p ac) oldParents frontPar
                  newLevels = foldl' (\ac v -> IM.insert v newLevel ac) oldLevels newFrontiers
                  bbfs = breadthFirstSearch (b { frontier = newFrontiers
                             , level = newLevels 
                             , parent = newParents
                             , maxLevel = newLevel
                             , topSort = (topSort b) ++ S.toList oldFrontiers
                             })

-- | BFS for graph with provided vertex adjacencyList
adjBFS :: IM.IntMap [Vertex] -> Vertex -> BFS
adjBFS neimap s = breadthFirstSearch sbfs
  where memoNeighbors v = IM.lookup v neimap
        sbfs = initialBFS s
        breadthFirstSearch b
          | S.empty == frontier b = b
          | otherwise = bbfs
            where oldLevel = maxLevel b
                  newLevel = oldLevel + 1
                  oldLevels = level b
                  oldFrontiers = frontier b
                  frontPar = let toCheck = foldl' (\ac v -> S.union ac (S.fromList (zip ((\mns -> case mns of 
                                                                                                    Nothing -> []
                                                                                                    Just ns -> ns) 
                                                                                        (memoNeighbors v)) (repeat v)))) S.empty oldFrontiers
                                in S.filter (\(n, p) -> not $ IM.member n oldLevels) toCheck
                  newFrontiers = S.map fst frontPar
                  oldParents = parent b
                  newParents = S.foldl' (\ac (n,p) -> IM.insert n p ac) oldParents frontPar
                  newLevels = foldl' (\ac v -> IM.insert v newLevel ac) oldLevels newFrontiers
                  bbfs = breadthFirstSearch (b { frontier = newFrontiers
                             , level = newLevels 
                             , parent = newParents
                             , maxLevel = newLevel
                             , topSort = (topSort b) ++ S.toList oldFrontiers
                             })

