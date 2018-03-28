{-|
Module      : BlumeCapel.Statistics
Description : Functions for gs record synthesis
Copyright   : Thodoris Papakonstantinou, 2018
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}


module Data.BlumeCapel.Statistics
  ( GSStats (..)
  , Stats (..)
  , gsStats
  , size
  ) where

import qualified Data.Aeson               as AE
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Text          (encodeToLazyText)
import qualified Data.ByteString.Lazy     as B
import qualified Data.Text                as TXT
import qualified Data.Text                as TXT
import qualified Data.Text.Encoding       as TEN
import qualified Data.Text.Lazy           as TXL
import           Data.Text.Lazy.IO        as I (appendFile, writeFile)
import qualified GHC.Generics             as GEN

import           Control.Applicative
import qualified Control.Foldl            as Fold
import           Data.Either
import qualified Data.IntMap.Strict       as IM
import           Data.List
import qualified Data.Map.Strict          as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio

import qualified Language.R.Instance      as R
import qualified Language.R.QQ            as HR

import qualified Data.BlumeCapel          as BC
import qualified Data.BlumeCapel.GSIO     as GSIO

data GSParams = GSParams
  { linear_size       :: !Int
  , dimensions        :: !Int
  , field             :: !Rational
  , disorder_strength :: !Rational
  , disorder_type     :: !String
  } deriving (Show, Ord, Eq, GEN.Generic)
instance AE.FromJSON GSParams
instance AE.FromJSONKey GSParams
instance AE.ToJSONKey GSParams
instance AE.ToJSON GSParams where
  toJSON pars =
    let fld = fromRational (field pars) :: Double
        dis = fromRational (disorder_strength pars) :: Double
     in AE.object [ "linear_size" AE..= linear_size pars
                  , "dimensions" AE..= dimensions pars
                  , "field" AE..= fld
                  , "disorder" AE..= dis
                  , "disorder_type" AE..= disorder_type pars
                  ]

-- | Container for computing mean stdErr
--Average n mean mean^2
data Accumulator a = Accumulator !Int !a !a
  deriving (Show, Eq, GEN.Generic)
instance Fractional a => Monoid (Accumulator a) where
  mempty = Accumulator 0 0 0
  mappend (Accumulator na ma ma2) (Accumulator nb mb mb2) =
    Accumulator n m m2
      where n = na + nb
            na' = fromIntegral na
            nb' = fromIntegral nb
            m = (na' * ma + nb' * mb) / (na' + nb')
            m2 = (na' * ma2 + nb' * mb2) / (na' + nb')

data ACC = ACC { mag  :: Accumulator Double
               , mag2 :: Accumulator Double
               , zcl2 :: Accumulator Double
               , ener :: Accumulator Double
               }
  deriving (Show, Eq, GEN.Generic)
instance Monoid ACC where
  mempty = ACC { mag = Accumulator 0 0 0
               , mag2 = Accumulator 0 0 0
               , zcl2 = Accumulator 0 0 0
               , ener = Accumulator 0 0 0
               }
  mappend l r = ACC { mag = mag l <> mag r
                    , mag2 = mag2 l <> mag2 r
                    , zcl2 = zcl2 l <> zcl2 r
                    , ener = ener l <> ener r
                    }

newtype GSACC = GSACC (M.Map GSParams ACC)
  deriving (Show, Eq, GEN.Generic)
instance Monoid GSACC where
  mempty = GSACC M.empty
  mappend (GSACC l) (GSACC r) = GSACC $
    M.unionWith (<>) l r

data Stats = Stats { n         :: !Int
                     , average :: !Double
                     , stdErr  :: !Double
  } deriving (Show, Eq, GEN.Generic)
instance AE.FromJSON Stats
instance AE.ToJSON Stats

recordsParameters :: GSIO.GSRecord -> GSParams
recordsParameters gr = GSParams
  { linear_size = GSIO.linear_size gr
  , dimensions = GSIO.dimensions gr
  , field = GSIO.field gr
  , disorder_strength = GSIO.disorder_strength gr
  , disorder_type = GSIO.disorder_type gr
  }

recordsToACC :: GSIO.GSRecord -> GSACC
recordsToACC rc =
  let pars = recordsParameters rc
      m = GSIO.magnetization $ GSIO.observables rc
      zrs = IM.toList $ GSIO.zeroclusters $ GSIO.observables rc
      -- | lattice size
      nn = (linear_size pars)^(dimensions pars)
      numclusters = length zrs
      l2 = if numclusters == 0
              then 0
              else (foldl'
                (\ac (s, n) -> ac + (fromIntegral n) * ((fromIntegral s)^2))
                0 zrs) / ((fromIntegral nn)^2) / (fromIntegral numclusters)
      l4 = if numclusters == 0
              then 0
              else (foldl'
                (\ac (s, n) -> ac + (fromIntegral n) * ((fromIntegral s)^4))
                0 zrs) / ((fromIntegral nn)^4) / (fromIntegral numclusters)
      en = GSIO.energy $ GSIO.observables rc
   in GSACC (M.singleton pars (ACC { mag = Accumulator 1 m (m^2)
                                   , mag2 = Accumulator 1 (m^4) (m^16)
                                   , zcl2 = Accumulator 1 l2 (l2^2)
                                   , ener = Accumulator 1 en (en^2)
                                   }))

getStats :: Accumulator Double -> Stats
getStats (Accumulator n x x2) =
  Stats { n = n
        , average = x
        , stdErr = sqrt $ (x2 - x^2) / (fromIntegral n)
        }

-- | need mean and mean^4 accumulators
getBinderCumulant :: Accumulator Double -> Accumulator Double -> Stats
getBinderCumulant (Accumulator n x x2) (Accumulator _ x4 x16) =
  let x2err =  sqrt $ (x4 - x2^2) / (fromIntegral n)
      x4err =  sqrt $ (x16 - x4^2) / (fromIntegral n)
{-
        2⋅m₄
m2' = ─────
          3
      3⋅m₂
      -1
m4'=  ─────
        2
    3⋅m₂
-}
   in Stats { n = n
            , average = 1 - (x4)/(3 *(x2^2))
            , stdErr = 1 -- ^ to be calculated
            }

data Observables = Observables
  { magnetization  :: Stats
  , binderCumulant :: Stats
  , l2             :: Stats
  } deriving (Show, Eq, GEN.Generic)
instance AE.FromJSON Observables
instance AE.ToJSON Observables

-- | Aggregates observables
newtype GSStats = GSStats (M.Map GSParams Observables)
  deriving (Show, Eq, GEN.Generic)
instance AE.FromJSON GSStats
instance AE.ToJSON GSStats
instance AE.ToJSONKey GSStats

accumulateRecords :: Fold.Fold (Either String GSIO.GSRecord) GSStats
accumulateRecords = Fold.Fold step begin done
  where
    begin = GSACC M.empty
    step l erc =
      case erc of
        Left _ -> l
        Right rc ->
          let nst = recordsToACC rc
           in l <> nst
    done (GSACC gaccu) =
      M.foldlWithKey
      (\(GSStats gsac) par x ->
        let m = mag x
            m2 = mag2 x
            l2 = zcl2 x
         in GSStats (M.insert par (Observables {magnetization = getStats m
                                               , binderCumulant = getBinderCumulant m m2
                                               , l2 = getStats l2
                                               }) gsac))
         (GSStats M.empty) gaccu

gsStats :: [Either String GSIO.GSRecord] -> GSStats
gsStats = Fold.fold accumulateRecords

size :: GSStats -> Int
size (GSStats s) = M.size s
