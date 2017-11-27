{-|
Module      : PushRelabel - Pure
Description : Maximum Flow - Min Cut - Push relabel algorithm - Pure
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Graph.PushRelabel.Pure
  ( ResidualGraph (..)
  , pushRelabel
  , netFlow
  , Network (..)
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as Set
import qualified Control.Monad.Parallel as Par
import Control.Monad

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import Data.Graph
import Data.Graph.Network
import Data.Graph.PushRelabel.Internal
import qualified Data.Graph.BFS as BFS

pushRelabel :: Network -> IO (Either String ResidualGraph)
pushRelabel net = do
  let initg = initializeResidualGraph net
  {-let res = initg-}
  let res = argalios initg 0
  let nvs = vertices $ graph $ network res
  let s = source net
  let t = sink net
  let insouts = filter (\v -> v /= s && v /= t && inflow res v < outflow res v) nvs
  let xsflows = filter (\v -> v /= s && v /= t && inflow res v - outflow res v /= excess res v) nvs
  let ofvs = IM.foldl (\ac ovs -> Set.union ac ovs) Set.empty $ overflowing res
  let notofvs = filter (\ ov -> 
                          let (ResidualVertex v l h x) = fromJust (IM.lookup ov (netVertices res)) 
                              ml = (IM.lookup l (overflowing res)) 
                           in case ml of
                                Nothing -> True
                                Just os -> not $ Set.member ov os
                       ) $ Set.toList $ getOverflowing $ netVertices res
  let errovfs = Set.filter (\v -> excess res v == 0) ofvs
  if null insouts && null xsflows && Set.null errovfs && null notofvs
      then return $ Right res
      else 
        if not $ null insouts 
              then return $ Left $ "Error Inflow < Outflow " ++ show insouts
              else
                if not $ null xsflows 
                  then return $ Left $ "Error vertex excess " ++ show xsflows
                  else
                    if not $ Set.null errovfs 
                      then return $ Left $ "Error not really overflowing " ++ show errovfs
                      else return $ Left $ "Error not in overflowing " ++ show notofvs
                        ++ " overflowings are " ++ show (overflowing res)
                        ++ " nevertices are " ++ show (netVertices res)

argalios :: ResidualGraph -> Int -> ResidualGraph 
argalios !rg steps = 
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      es = edges g
      vs = vertices g
      olf = netFlow rg
      !rg' = prePush $ prePull $ bfsRelabel rg
      nfl = netFlow rg'
      steps' = steps + 1
      oovfls = overflowing rg
      novfls = overflowing rg'
   in if nfl == olf 
         then 
           if oovfls == novfls
              then rg' {steps = steps'}
              else argalios rg' steps'
         else argalios rg' steps'
{-# INLINE argalios #-}

-- | pushes flow though edges with starting vertices which are the ends of source edges 
-- (U) and ending edges that are the start of sink edges (V)
prePush :: ResidualGraph -> ResidualGraph 
prePush rg = 
  let ovfs = overflowing rg
   in IM.foldl' (\ac lset -> 
         Set.foldl' (\ac' v -> pushNeighbors ac' v)
         ac lset
      ) rg ovfs

prePull :: ResidualGraph -> ResidualGraph 
prePull rg = 
  let ovfs = overflowing rg
   in IM.foldr' (\lset ac -> 
         Set.foldl' (\ac' v -> pullNeighbors ac' v)
         ac lset
               ) rg ovfs

pushNeighbors :: ResidualGraph -> Vertex -> ResidualGraph
pushNeighbors g v =
  let neimap = netNeighborsMap g
      xv = excess g v
      (fns, rns) = fromJust $ IM.lookup v neimap
      feds = map (\n -> fromTuple (v,n)) fns
   in foldl' (\ac e -> 
                let mv = push ac e
                in case mv of 
                    Nothing -> ac
                    Just g'' -> g'') g feds

pullNeighbors :: ResidualGraph -> Vertex -> ResidualGraph
pullNeighbors g v =
  let neimap = netNeighborsMap g
      (fns, rns) = fromJust $ IM.lookup v neimap
      reds = map (\n -> fromTuple (n,v)) rns
      xv = excess g v
   in foldl' (\ac e -> 
                let mv = pull ac e
                 in case mv of 
                      Nothing -> ac
                      Just g'' -> g'') g reds

bfsRelabel :: ResidualGraph -> ResidualGraph
bfsRelabel rg =
  let sh = numVertices g
      (tlvs,slvs) = residualDistances
      rg' = foldl' (\ ac (v,l) -> 
             let h = sh + l
              in updateHeight ac v h
                ) rg $ IM.toList slvs 
      rg'' = foldl' (\ ac (v,h) -> updateHeight ac v h
                    ) rg' $ IM.toList tlvs
  in rg''
  where
    g = graph $ network rg
    s = source $ network rg
    t = sink $ network rg
  -- | (distance from sink , distance from source (only those that don't connect
  -- to sink))
    residualDistances = 
      let es = map snd (IM.toList $ netEdges rg)
          tres = filter (\(ResidualEdge e c f) -> f < c) es
          tbes = filter (\(ResidualEdge e c f) -> f > 0) es
          tfsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
            let u = from e
                v = to e 
                mns = IM.lookup v ac 
             in case mns of 
                   Nothing -> IM.insert v [u] ac
                   Just ns -> IM.insert v (u:ns) ac
                 ) IM.empty tres
          tsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
            let u = from e
                v = to e 
                mns = IM.lookup u ac 
             in case mns of 
                   Nothing -> IM.insert u [v] ac
                   Just ns -> IM.insert u (v:ns) ac
                 ) tfsatnbs tbes
          sfsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
            let u = from e
                v = to e 
                mns = IM.lookup v ac 
             in case mns of 
                   Nothing -> IM.insert u [v] ac
                   Just ns -> IM.insert u (v:ns) ac
                 ) IM.empty tres
          ssatnbs = foldl' (\ac (ResidualEdge e c f) -> 
            let u = from e
                v = to e 
                mns = IM.lookup v ac 
             in case mns of 
                   Nothing -> IM.insert v [u] ac
                   Just ns -> IM.insert v (u:ns) ac
                 ) sfsatnbs tbes
          tlvs = BFS.level $ BFS.adjBFS tsatnbs t
          slvs = BFS.level $ BFS.adjBFS ssatnbs s
        in (tlvs,slvs)

