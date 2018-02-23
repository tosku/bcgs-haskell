{-|
Module      : BlumeCapel.GSIO
Description : Jobfile definition, persistance of results and serialization
Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

- GSRecord the result container for a single ground state

 -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Data.BlumeCapel.GSIO where

import Data.List
import Data.Maybe
import Data.Either.Unwrap
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntSet as Set
import qualified Data.IntMap.Strict as IM

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I (appendFile,writeFile)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as TXT
import Data.Text.Encoding
import           System.Environment
import qualified Data.ByteString.Lazy  as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Bits as Bits
import           GHC.Generics
import           System.IO
import           System.Posix
import           System.Posix.IO.ByteString as BS
import Control.Parallel.Strategies
import qualified Control.Monad.Parallel as MPar
import Control.Concurrent.STM
import Control.Concurrent.ParallelIO.Global

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import Data.Graph
import qualified Data.Graph.Grid as Lat
import qualified Data.BlumeCapel as BC
import Data.BlumeCapel.GSNetwork
import Data.Graph.PushRelabel.Pure
{-import qualified Data.Graph.PushRelabel.STM as IOPR-}

import Data.PRNG
import Data.PRNG.MTRNG

default (Int,Rational,Double)

data ParamRange = ParamRange 
  { rangefrom :: Double
  , rangeto :: Double
  , rangestep :: Double
  } deriving (Show, Generic)
instance FromJSON ParamRange
instance ToJSON ParamRange
data JobArguments = JobArguments
  { _l :: Int
  , _d :: Int
  , _delta :: ParamRange
  , _disorder :: ParamRange
  , _disorderType :: String
  , _realizations :: Int
  , _seedofSeeds :: Int
  , _resultfile    :: String
  } deriving (Show, Generic)
instance FromJSON JobArguments
instance ToJSON JobArguments

data Observables = Observables
  { energy :: Double
  , magnetization :: Double
  , configuration :: BC.BCConfiguration
  } deriving (Show, Generic)
instance ToJSON Observables where
  toJSON obs = object ["energy" .= energy obs
                      , "mag" .= magnetization obs 
                      , "configuration" .= encodeConf (configuration obs)]

encodeConf :: BC.BCConfiguration -> String
encodeConf (BC.SpinConfiguration conf) = 
  let intlist = map (\(k,s) -> floor (fromRational (BC.project BC.referenceSpin s))::Int) $
                      IM.toList conf
      {-isres = foldl' (\(i,ac) x -> -}
        {-(i+1, if x==1 then -}
                   {-Bits.setBit ac i -}
                   {-else ac)) (0,Bits.zeroBits) intlist -}
   {-in snd isres-}
  in foldr (\x ac-> 
            let c = show x
             in c ++ ac)
         "" intlist


data GSRecord = GSRecord
  { linear_size :: Int
  , dimensions :: Int
  , field :: Rational
  , disorder_strength :: Rational
  , disorder_type :: String
  , realization_id :: Int
  , observables :: Observables
  } deriving (Show, Generic)
instance ToJSON GSRecord

data GSParams = GSParams
  { l :: Lat.L 
  , d :: Lat.D 
  , r :: BC.DisorderStrength 
  , disorderType :: String 
  , delta :: BC.Delta 
  , seed :: Int
  } deriving (Show,Eq,Ord)

getGS :: GSParams -> GroundState
getGS params =
  let distype = if (disorderType params) == "unimodal"
      then BC.Unimodal
      else BC.Dichotomous
      latt = Lat.graphCubicPBC $ Lat.PBCSquareLattice (l params) (d params)
      rbbc = BC.RandomBond { BC.bondDisorder = distype (seed params) (r params)
                           , BC.crystalField = (delta params)
                           }
      real = BC.realization'RBBC rbbc latt
   in groundState real

saveGS :: GSParams -> GroundState -> GSRecord
saveGS args gs =
  let nvs = fromIntegral $ (numVertices $ BC.lattice $ BC.realization $ replica gs)
      en = fromRational $ BC.energy $ replica gs
      mag = (fromRational $ BC.getMagnetization $ BC.configuration $ replica gs) / nvs :: Double
      conf = BC.configuration $ replica gs
      gsrec = GSRecord
                { linear_size = fromIntegral $ l args
                , dimensions = fromIntegral $ d args
                , field =  delta args
                , disorder_strength =  r args
                , disorder_type = disorderType args
                , realization_id = seed args
                , observables = Observables 
                  { energy = en
                  , magnetization = mag
                  , configuration = conf
                  }
                }
   in gsrec


getRange :: ParamRange -> [Rational]
getRange params =
     let frm = toRational $ rangefrom params
         nxt = frm + toRational (rangestep params)
         rend = toRational $ rangeto params
      in [frm,nxt..rend]

argumentsToParameters :: JobArguments 
                      -> Either String [GSParams]
argumentsToParameters args = 
     let size = (fromIntegral $ _l args) :: Lat.L
         dimensions = fromIntegral $ _d args
         distype = _disorderType args
         rs = getRange $ _disorder args
         deltas = getRange $ _delta args
         seeds = randomInts (getRNG $ _seedofSeeds args :: MTRNG) (_realizations args)
      in if not $ elem distype ["unimodal", "dichotomous"]
            then Left "Available bond disorder unimodal, dichotomous"
            else Right $ map (\(s, (r,d)) -> 
              GSParams { l = size
                       , d = dimensions
                       , r = r
                       , disorderType = distype
                       , delta = d
                       , seed = s
                       }) $ (,) <$> seeds <*> ((,) <$> rs <*> deltas)

runJobSTM :: [GSParams] -> IO [TVar (GSParams, GroundState)]
runJobSTM pars = do
  let gss = map getGS pars
  mapM newTVarIO (zip pars gss)
         
{-runJobIO :: [GSParams] -> IO [(GSParams, GroundState)]-}
{-runJobIO pars = do-}
  {-putStrLn $ show $ length pars-}
  {-results <- runJobSTM pars-}
  {-parallel $ map (\x -> atomically (readTVar x)) results-}

runJobIO :: [GSParams] -> IO [(GSParams, GroundState)]
runJobIO pars = do
  putStrLn $ show $ length pars
  parallel $ map (\par -> do
    let gs = getGS par
    putStrLn $ show (cutEnergy gs)
    return $ (par, gs)) pars

type Results = M.Map GSParams GroundState

runJob :: [GSParams] -> IO Results
runJob pars = do
  putStrLn $ show $ length pars
  results <- newTVarIO (M.empty :: Results)
  let updateResults :: GSParams -> GroundState -> STM ()
      updateResults par gs = do
        res <- readTVar results
        let res' = M.insert par gs res
        writeTVar results res'
  let getResults :: IO Results
      getResults = do
        res <- atomically $ readTVar results
        return res
  parallel_ 
    $ map (\par -> do 
      let gs = getGS par
          !mag = getGSMag gs
          !en = getGSEnergy gs
      atomically $ updateResults par gs
      res <- getResults
      let pr = M.size res
      hPutChar stdout '\r' 
      hPutStr stdout $ show pr ++ "    "
      hFlush stdout
      ) pars
  getResults

gsToJSON :: Results -> [GSRecord]
gsToJSON gss = map (\(par,gs) -> saveGS par gs) $ M.toList gss

getJson :: ToJSON a => a -> String
getJson d = TXT.unpack $ decodeUtf8 $ BSL.toStrict (encodePretty d)

{-sumRealizations :: Results -> Delta -> DisorderStrength -> [(,)]-}

