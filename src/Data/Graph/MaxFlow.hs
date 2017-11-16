{-|
Module      : PushRelabel
Description : Maximum Flow - Min Cut - Push relabel algorithm
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Graph.MaxFlow
  ( ResidualGraph (..)
  , pushRelabel
  , initializeResidualGraph
  , ResidualVertex (..)
  , ResidualEdge (..)
  , Network (..)
  , Capacity (..)
  , Capacities (..)
  , Flow 
  , excess
  , height
  {-, inFlow-}
  , netFlow
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as Set
import qualified Control.Monad.Parallel as Par
import Control.Concurrent.STM
import Control.Monad

import Data.Graph

type Capacity = Rational 
type Capacities = M.Map Edge Capacity 
type Flow = Capacity
type Height = Natural
type Excess = Capacity
data ResidualVertexP = ResidualVertexP Vertex Height Excess
  deriving (Show,Eq)

data ResidualVertex = ResidualVertex !Vertex !(TVar Height) !(TVar Excess)
  deriving (Eq)

data ResidualEdgeP = ResidualEdgeP Edge Capacity Flow
  deriving (Show,Eq)

data ResidualEdge = ResidualEdge !Edge !Capacity !(TVar Flow)
  deriving (Eq)


type ResidualVertices = IM.IntMap ResidualVertex
type NeighborsMap = IM.IntMap ([Vertex],[Vertex])
type ResidualEdges = IM.IntMap ResidualEdge

data Network = Network { graph :: Graph
                       , source :: Vertex
                       , sink :: Vertex
                       , capacities :: Capacities
                       , flow :: Capacities
                       }
                       deriving (Show,Eq)

data ResidualGraph = ResidualGraph { network :: Network
                                   , netVertices :: ResidualVertices
                                   , netEdges :: ResidualEdges 
                                   , netNeighborsMap :: IM.IntMap ([Vertex], [Vertex])
                                   , steps :: Int
                                   }
                       deriving (Eq)

initializeResidualGraph :: Network -> IO ResidualGraph
initializeResidualGraph net = do
  vs <- initializeVertices net
  es <- initializeEdges net
  return ResidualGraph { network = net
                       , netVertices = vs
                       , netEdges = es
                       , netNeighborsMap = getNetNeighborsMap $ graph net
                       , steps = 0
                       }

getNetNeighborsMap :: Graph -> IM.IntMap ([Vertex],[Vertex])
getNetNeighborsMap g =
  let neis v = (neighbors g v, fromJust (IM.lookup v (getReverseNeighbors (vertices g) (edges g))))
   in foldl (\ac v -> IM.insert v (neis v) ac) IM.empty (vertices g)

netNeighbors :: NeighborsMap -> Vertex -> ([Vertex],[Vertex]) -- ^ graph and reverse (inward and outward) neighbors
netNeighbors nm v = fromJust $ IM.lookup v nm

getReverseNeighbors :: [Vertex] -> [Edge] -> IM.IntMap [Vertex]
getReverseNeighbors vs es = IM.fromList $ zip vs (map (\v -> map from (filter (\re -> to re == v) es)) vs)

sourceEdges :: Network -> [(Edge,Capacity)]
sourceEdges net = 
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      cap v = fromJust $ M.lookup (Edge s v) cs
    in map (\v -> ((Edge s v), cap v )) (neighbors g s) 

initializeVertices :: Network -> IO ResidualVertices
initializeVertices net = do
  let g = graph net
  let cs = capacities net
  let s = source net
  let t = sink net
  let sh = fromIntegral $ numVertices g
  let ses = sourceEdges net
  let zvs = IM.fromList $ zip (vertices g) (map (\v -> ResidualVertexP v 1 0) $ vertices g)
  let (sx, nvs) = foldl' (\(cx,ac) (e,c) -> let v = to e
                                              in (cx-c, IM.insert v (ResidualVertexP v 2 c) ac)) (0, zvs) ses
  let fvs = IM.insert s (ResidualVertexP s sh sx) nvs
  traverse (\(ResidualVertexP v h x) -> do 
                                      th <- newTVarIO h
                                      tx <- newTVarIO x
                                      return $ ResidualVertex v th tx
           ) fvs

initializeEdges :: Network -> IO ResidualEdges
initializeEdges net = do
  let g = graph net
  let cs = capacities net
  let s = source net
  let t = sink net
  let inites = IM.fromList $ map (\(e,c) -> (fromJust $ edgeIndex g e, ResidualEdgeP e c 0)) (M.toList cs)
  let ses = sourceEdges net
  let fes = foldl' (\ac (e,c) -> IM.insert (fromJust $ edgeIndex g e) (ResidualEdgeP e c c) ac) inites ses 
  traverse (\(ResidualEdgeP e c f) -> do
                                      tf <- newTVarIO f
                                      return $ ResidualEdge e c tf
           ) fes

overflowing :: ResidualGraph -> IO [Vertex]
overflowing rg = atomically $ do
  let vs = vertices $ graph $ network rg
  let s = source $ network rg
  let t = sink $ network rg
  xvs <- mapM (\v -> fmap ((,) v) (excess rg v)) vs
  return $ map fst $ filter (\(v,x) -> x/=0 && v /= s && v /= t) xvs

pushRelabel :: Network -> IO Flow
pushRelabel net = do
  initg <- initializeResidualGraph net
  !ovfs <- fmap Set.fromList $ overflowing initg
  !res <- prl initg ovfs 0
  {-mapM_ (\v -> printVertex initg v) vs-}
  {-let nvs = filter (\(ResidualVertexP v h x) -> x/=0) $ fst res-}
  {-{-let nvs =  fst res-}-}
  {-print $ map (\(ResidualVertexP v' h x) -> -}
    {-let v = (ResidualVertexP v' h x) -}
     {-in (v', fromRational x - (inflowP initg (snd res) v) + (outflowP initg (snd res) v))) nvs-}
  netFlow initg
  where 
      g = graph net
      cs = capacities net
      s = source net
      t = sink net
      vs = vertices g
      innervs = filter (\v -> v/=s && v/=t) $ vertices g
      prl :: ResidualGraph -> Set.IntSet -> Int -> IO ([ResidualVertexP],[ResidualEdgeP])
      prl rg ovfs steps =
        if Set.null ovfs 
           then do
             es <- atomically $ mapM (\e -> do
                     let c = edgeCapacity rg e
                     f <- edgeFlow rg e 
                     return $ ResidualEdgeP e c f
                                     ) 
                   (edges (graph (network rg)))
             vs <- atomically $ mapM (\v -> do
                     h <- height rg v 
                     x <- excess rg v 
                     return $ ResidualVertexP v h x
                                     ) 
                   (vertices (graph (network rg)))
             return (vs,es)
           else do
             !lovfs <- Par.mapM (\v -> do
                           !os <- atomically $
                             do
                                 !x <- excess rg v
                                 if x > 0 
                                    then do
                                       orElse 
                                         (discharge rg v)
                                         ((relabel rg v) >> return [v])
                                    else return []
                           return os
                                ) $ Set.toList ovfs
             let !ovfs' = Set.difference (Set.unions $ map Set.fromList lovfs) $ Set.fromList [s,t]
             {-if steps > fromIntegral t -}
                {-then do-}
                 {-ovfs'' <- fmap Set.fromList $ overflowing rg-}
                 {-print steps-}
                 {-print $ Set.difference ovfs' ovfs''-}
               {-else return ()-}
             prl rg ovfs' (steps +1)
                  





discharge :: ResidualGraph -> Vertex -> STM [Vertex]
discharge g v = do
  let neimap = netNeighborsMap g
  let (fns, rns) = fromJust $ IM.lookup v neimap
  let feds = map (\n -> fromTuple (v,n)) fns
  let reds = map (\n -> fromTuple (n,v)) rns
  !changed <- foldM (\ac e -> fmap (\mv -> 
        case mv of 
          Nothing -> ac
          Just v -> v:ac) (push g e)) [] feds
  !changed' <- foldM (\ac e -> fmap (\mv -> 
        case mv of 
          Nothing -> ac
          Just v -> v:ac) (pull g e)) changed reds
  if null changed' then 
                retry
              else
                return changed

push :: ResidualGraph -> Edge -> STM (Maybe Vertex)
push g e = do 
  let u = from e
  let v = to e
  hu <- height g u
  hv <- height g v
  xu <- excess g u 
  xv <- excess g v
  let c = edgeCapacity g e
  f <- edgeFlow g e
  let nvs = netVertices g
  let xf = min xu (c - f)
  if (hu >= hv + 1) && xf > 0 then do
    updateEdge g e (f + xf)
    updateVertex g u hu (xu - xf)
    updateVertex g v hv (xv + xf)
    return $ Just v
  else return Nothing

pull :: ResidualGraph -> Edge -> STM (Maybe Vertex)
pull g e = do 
  let u = from e
  let v = to e
  hu <- height g u
  hv <- height g v
  xu <- excess g u 
  xv <- excess g v
  let c = edgeCapacity g e
  f <- edgeFlow g e
  let nvs = netVertices g
  let xf = min xv f
  if (hv >= hu + 1)  && xf > 0 then do 
    updateEdge g e (f - xf)
    updateVertex g u hu (xu + xf)
    updateVertex g v hv (xv - xf)
    return $ Just u
  else return Nothing

relabel :: ResidualGraph -> Vertex -> STM ()
relabel g v = do
  let ns  = netNeighbors (netNeighborsMap g) v 
  let gnes = map (\n -> fromTuple (v,n)) $ fst ns -- ^ neighboring graph edges
  let rnes = map (\n -> fromTuple (n,v)) $ snd ns -- ^ neighboring reverse edges
  gacfs <- mapM (\e -> fmap ((\f -> (e,(-) (edgeCapacity g e) f))) (edgeFlow g e)) gnes
  let gcfs = filter (\(e,cf) -> cf > 0) gacfs
  racfs <- mapM (\e -> fmap ((\f -> (e,f))) (edgeFlow g e)) rnes
  let rcfs = filter (\(e,cf) -> cf > 0) racfs
  hv <- height g v
  xv <- excess g v
  neighborHeights <- (++) <$> (mapM (\(e,c) -> height g (to e)) gcfs) <*> mapM (\(e,c) -> height g (from e)) rcfs
  let newh = 1 + minimum neighborHeights
  case any (\nh -> hv > nh) neighborHeights || neighborHeights == [] of
     False -> do
       updateVertex g v newh xv
     True -> return ()

updateVertex :: ResidualGraph -> Vertex -> Height -> Excess -> STM ()
updateVertex g v nh nx = do 
  let nv = fromJust $ IM.lookup v (netVertices g)
      s = source $ network g
      t = sink $ network g
  if v /= s && v /= t then do
    updateHeight nv nh
    updateExcess nv nx
  else 
    return ()

updateHeight :: ResidualVertex -> Height -> STM ()
updateHeight (ResidualVertex v h x) nh = writeTVar h nh

updateExcess :: ResidualVertex -> Excess -> STM ()
updateExcess (ResidualVertex v h x) nx = writeTVar x nx

updateEdge :: ResidualGraph -> Edge -> Flow -> STM ()
updateEdge g e f = do
  let l = graph $ network g
  let eid = fromJust $ edgeIndex l e
  let es = netEdges g
  let (ResidualEdge oe c tf) = fromJust $ IM.lookup eid es
  writeTVar tf f

netFlow :: ResidualGraph -> IO Flow
netFlow g = atomically $ do
  fl <- inflow g (sink (network g))
  return fl

vertex :: ResidualVertex -> Vertex
vertex (ResidualVertex v _ _) = v

height :: ResidualGraph -> Vertex -> STM Height
height rg v = do 
  let g = graph $ network rg
  let s = source $ network rg
  let t = sink $ network rg
  let nvs = fromIntegral $ numVertices g
  let (ResidualVertex nv h e) = fromJust $ IM.lookup v (netVertices rg)
  if v == s 
     then do
        return nvs
      else
        if v == t 
           then do
             return 0
            else
              readTVar h

excess :: ResidualGraph -> Vertex -> STM Excess
excess rg v = do 
  let g = graph $ network rg
  let s = source $ network rg
  let t = sink $ network rg
  let nvs = fromIntegral $ numVertices g
  let (ResidualVertex nv h e) = fromJust $ IM.lookup v (netVertices rg)
  if v == s 
     then do
        return 0
      else
        if v == t 
           then do
             return 0
            else
              readTVar e


edgeCapacity :: ResidualGraph -> Edge -> Capacity
edgeCapacity g e = let (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ edgeIndex (graph $ network g) e) (netEdges g)
                    in c

edgeFlow :: ResidualGraph -> Edge -> STM Flow
edgeFlow g e = do
  let l = graph $ network g
  let (ResidualEdge ne c tf) = fromJust $ IM.lookup (fromJust $ edgeIndex l e) (netEdges g)
  readTVar tf

printVertex :: ResidualGraph -> Vertex -> IO ()
printVertex rg v = do
  (h ,x ,inf ,ouf ) <- atomically $ do
    x' <- excess rg v
    h' <- height rg v
    inf' <- inflow rg v
    ouf' <- outflow rg v
    return (h',x',inf',ouf')
  putStrLn $ show v 
    ++ " h: "++ show h 
    ++ ", exc: " ++ show (fromRational x) 
    ++ ", infl: " ++ show (fromRational inf)
    ++ ", outf: " ++ show (fromRational ouf) 

printEdge :: ResidualGraph -> Edge -> IO ()
printEdge rg e = do
  (c,f) <- atomically $ do
    let c' = edgeCapacity rg e
    f' <- edgeFlow rg e
    return (c',f')
  putStrLn $ show e 
    ++ " c: "++ show (fromRational c) 
    ++ ", f: " ++ show (fromRational f) 

inflowP :: ResidualGraph -> [ResidualEdgeP] -> ResidualVertexP -> Double
inflowP g es (ResidualVertexP v h x) =
  let ns  = netNeighbors (netNeighborsMap g) v 
      reds = map (\n -> fromTuple (n,v)) $ snd ns
      xf = foldl' (\ac e ->
                            let (ResidualEdgeP e'' c f) = fromJust $ find (\(ResidualEdgeP e' c f) -> e' == e ) es
                             in (f + ac)
                  ) 0 reds
               in fromRational xf

outflowP :: ResidualGraph -> [ResidualEdgeP] -> ResidualVertexP -> Double
outflowP g es (ResidualVertexP v h x) =
  let ns  = netNeighbors (netNeighborsMap g) v 
      feds = map (\n -> fromTuple (v,n)) $ fst ns
      xf = foldl' (\ac e ->
                            let (ResidualEdgeP e'' c f) = fromJust $ find (\(ResidualEdgeP e' c f) -> e' == e ) es
                             in (f + ac)
                  ) 0 feds
               in fromRational xf

inflow :: ResidualGraph -> Vertex -> STM Flow
inflow g v = do
  let ns  = netNeighbors (netNeighborsMap g) v 
  let reds = map (\n -> fromTuple (n,v)) $ snd ns
  foldM (\ac e -> ((+) ac) <$> edgeFlow g e) 0 reds 

outflow :: ResidualGraph -> Vertex -> STM Flow
outflow g v = do
  let ns  = netNeighbors (netNeighborsMap g) v 
  let feds = map (\n -> fromTuple (v,n)) $ fst ns
  foldM (\ac e -> ((+) ac) <$> edgeFlow g e) 0 feds 

{-pushRelabel :: Network -> IO Flow-}
{-pushRelabel net = do-}
  {-initg <- initializeResidualGraph net-}
  {-Par.mapM_ (\v -> prl initg v 0 (-1) 0) innervs-}
  {-mapM_ (\v -> printVertex initg v) vs-}
  {-netFlow initg-}
  {-where -}
      {-g = graph net-}
      {-cs = capacities net-}
      {-s = source net-}
      {-t = sink net-}
      {-vs = vertices g-}
      {-innervs = filter (\v -> v/=s && v/=t) $ vertices g-}
      {-prl :: ResidualGraph -> Vertex -> Excess -> Height -> Int -> IO ()-}
      {-prl rg v x h s = do-}
        {-!(ix,ih) <- atomically $ -}
                     {-do-}
                       {-x' <- excess rg v-}
                       {-h' <- height rg v-}
                       {-{-if x == x' && h == h' then-}-}
                                    {-{-retry-}-}
                                  {-{-else do-}-}
                       {-if x' > 0 then do-}
                         {-orElse-}
                           {-(discharge rg v)-}
                           {-(relabel rg v)-}
                           {-else return ()-}
                       {-x'' <- excess rg v-}
                       {-h'' <- height rg v-}
                       {-return (x'',h'')-}
        {-if s > fromIntegral t then do-}
             {-{-printVertex rg v-}-}
             {-return ()-}
           {-else -}
             {-return ()-}
             {-{-printVertex rg v-}-}
        {-{-if (ix == 0 && ih > (fromIntegral t)+1) || s > fromIntegral t then do-}-}
        {-if (ix == 0 && ih >= (fromIntegral t)) then do-}
                             {-return ()-}
                           {-else-}
                             {-prl rg v ix ih (s+1)-}


