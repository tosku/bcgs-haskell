{-|
Module      : BlumeCapel.GSIO
Description : Jobfile definition, persistance of results and serialization
Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Data.BlumeCapel.GSIO where

import           Data.Either.Unwrap
import qualified Data.IntMap.Strict                           as IM
import qualified Data.IntSet                                  as Set
import           Data.List
import qualified Data.Map                                     as M
import           Data.Maybe
import           Data.Monoid

import qualified Codec.Binary.UTF8.String                     as CU
import           Control.Concurrent.ParallelIO.Global
import           Control.Concurrent.STM
import qualified Data.Aeson                                   as AE
import           Data.Aeson.Encode.Pretty                     (encodePretty)
import           Data.Aeson.Text                              (encodeToLazyText)
import qualified Data.Bits                                    as Bits
import qualified Data.ByteString.Char8                        as C
import qualified Data.ByteString.Lazy                         as B
import qualified Data.Text                                    as TXT
import qualified Data.Text.Encoding                           as TEN
import qualified Data.Text.Lazy                               as TXL
import           Data.Text.Lazy.IO                            as I (appendFile,
                                                                    writeFile)
import qualified Data.Time                                    as Time
import           GHC.Generics
import           System.Environment
import           System.IO
import qualified System.Posix                                 as SP
import qualified System.Posix.Files                           as PF
import qualified System.Posix.IO                              as PIO
import           "unix-bytestring" System.Posix.IO.ByteString (fdPread,
                                                               fdPwrite)

import           Data.Graph.AdjacencyList
import qualified Data.Graph.AdjacencyList.Grid                as Lat
import           Data.Graph.AdjacencyList.PushRelabel.Pure

import qualified Data.BlumeCapel                              as BC
import           Data.BlumeCapel.GSNetwork

import           Data.PRNG
import           Data.PRNG.MTRNG

default (Int,Rational,Double)

data ParamRange = ParamRange
  { rangefrom :: Double
  , rangeto   :: Double
  , rangestep :: Double
  } deriving (Show, Generic)
instance AE.FromJSON ParamRange
instance AE.ToJSON ParamRange
data JobArguments = JobArguments
  { _l            :: Int
  , _d            :: Int
  , _delta        :: ParamRange
  , _disorder     :: ParamRange
  , _disorderType :: String
  , _realizations :: Int
  , _seedofSeeds  :: Int
  , _resultfile   :: String
  } deriving (Show, Generic)
instance AE.FromJSON JobArguments
instance AE.ToJSON JobArguments

data Observables = Observables
  { energy        :: Double
  , magnetization :: Double
  , zeroclusters  :: BC.ZeroDistribution
  } deriving (Show, Generic)
instance AE.FromJSON Observables
instance AE.ToJSON Observables

data GSRecord = GSRecord
  { linear_size       :: !Int
  , dimensions        :: !Int
  , field             :: !Rational
  , disorder_strength :: !Rational
  , disorder_type     :: !String
  , realization_id    :: !Int
  , observables       :: !Observables
  } deriving (Show, Generic)
instance AE.ToJSON GSRecord
instance AE.FromJSON GSRecord

data GSParams = GSParams
  { l            :: Lat.L
  , d            :: Lat.D
  , r            :: BC.DisorderStrength
  , disorderType :: String
  , delta        :: BC.Delta
  , seed         :: Int
  } deriving (Show,Eq,Ord)

getGS :: GSParams -> GroundState
getGS params =
  let distype = if (disorderType params) == "unimodal"
      then BC.Unimodal
      else BC.Dichotomous
      latt = Lat.graphCubicPBC $ Lat.PBCSquareLattice (l params) (d params)
      rbbc = BC.RandomBond
           { BC.bondDisorder = distype (seed params) (r params) (delta params)
           , BC.crystalField = (delta params)
           }
      real = BC.realization'RBBC rbbc latt
   in groundState real

saveGS :: GSParams -> GroundState -> GSRecord
saveGS args gs =
  let nvs = fromIntegral $ (numVertices $ BC.lattice $ BC.realization $ replica gs)
      en = (fromRational $ BC.energy $ replica gs) / nvs :: Double
      mag = (fromRational $ BC.getMagnetization $ BC.configuration $ replica gs) / nvs :: Double
      zclusters = BC.zeroCusterSizes $ replica gs
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
                  , zeroclusters = zclusters
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

getJson :: AE.ToJSON a => a -> String
getJson d = TXT.unpack $ TEN.decodeUtf8 $ B.toStrict (encodePretty d)

runJob :: String -> IO ()
runJob jobfilename = do
  let jobfile = B.readFile jobfilename
  readParams <- (AE.eitherDecode <$> jobfile) :: IO (Either String JobArguments)
  case readParams of
       Left err -> do
           putStrLn $ "problem with the job file" ++ err
       Right args -> do
          putStrLn "job file"
          putStrLn $ (getJson args)
          let epars = argumentsToParameters args
          case epars of
            Left err ->
              putStrLn $ "problem with the job file" ++ err
            Right pars -> do
              putStrLn "running"
              putStrLn $ "total: " ++ (show $ length pars)
              currentTime <- Time.getCurrentTime
              let file = (show currentTime) ++ "_" ++ _resultfile args
              buffer <- newTVarIO (0,0,0,C.empty)
                :: IO (TVar (Int, SP.COff, SP.COff, C.ByteString))
              resfd <- PIO.createFile file PF.ownerModes
              let writeResults :: GSParams -> GroundState -> IO Int
                  writeResults par gs = do
                    let res = saveGS par gs
                    !(d,newofs, ofs, b) <- atomically $ do
                          (done, offset, offsetold, bf) <- readTVar buffer
                          let !resbyt = TEN.encodeUtf8
                                      $ TXL.toStrict
                                      $ (encodeToLazyText res) <> TXL.pack "\n"
                              !bf' = C.append resbyt bf
                              !offset' = (fromIntegral $ C.length bf') + offset
                              !done' = done + 1
                          writeTVar buffer (done', offset', offset, C.empty)
                          return (done', offset', offset, bf')
                    fdPwrite resfd b ofs
                    return d
              parallel_
                $ map (\par -> do
                        let !gs = getGS par
                        pr <- writeResults par gs
                        hFlush stdout
                        hPutChar stdout '\r'
                        hPutStr stdout $ "done " ++ (show pr)
                          ++ " mag: " ++ (show $ getGSMag gs)
                        hFlush stdout
                      ) pars
              return ()

readResults :: String -> IO (Either String [GSRecord])
readResults resultsfilename = do
  inh <- openFile resultsfilename ReadMode
  reslines <- readlines inh []
  hClose inh
  let results = foldr (\x ac -> fmap (:) x <*> ac) (Right [])
              $ map (AE.eitherDecode . B.pack . CU.encode) reslines
  return results
  where
    readlines :: Handle -> [String] -> IO [String]
    readlines inh res = do
      ineof <- hIsEOF inh
      if ineof
        then return res
        else do
          inpStr <- hGetLine inh
          let res' = inpStr : res
          readlines inh res'
