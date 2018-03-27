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
import           Data.Monoid
import qualified Control.Foldl            as Fold
import           Data.Either
import qualified Data.IntMap.Strict       as IM
import           Data.List
import qualified Data.Map.Strict          as M
import           Data.Maybe
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

recordsParameters :: GSIO.GSRecord -> GSParams
recordsParameters gr = GSParams
  { linear_size = GSIO.linear_size gr
  , dimensions = GSIO.dimensions gr
  , field = GSIO.field gr
  , disorder_strength = GSIO.disorder_strength gr
  , disorder_type = GSIO.disorder_type gr
  }

recordsObservables :: GSIO.GSRecord -> Observables
recordsObservables gr = Observables
  { magnetization = GSIO.magnetization $ GSIO.observables gr
  }

-- | Mean and standard error
data Averaged = Averaged
  { mean   :: Double
  , stdErr :: Double
  } deriving (Show, Eq, GEN.Generic)
instance AE.FromJSON Averaged
instance AE.ToJSON Averaged

data Observables = Observables
  { magnetization :: Double
  } deriving (Show, Eq, GEN.Generic)
instance AE.FromJSON Observables
instance AE.ToJSON Observables

-- | Aggregates observables
newtype GSStats = GSStats (M.Map GSParams Observables)
  deriving (Show, Eq, GEN.Generic)
instance AE.FromJSON GSStats
instance AE.ToJSON GSStats
instance AE.ToJSONKey GSStats

instance Monoid GSStats where
  mempty = GSStats M.empty
  mappend (GSStats l) (GSStats r) = GSStats $
    M.unionWith (\ ol or ->
    Observables { magnetization = (magnetization ol + magnetization or)})
    l r

accumulateRecords :: Fold.Fold (Either String GSIO.GSRecord) GSStats
accumulateRecords = Fold.Fold step begin id
  where 
    begin = GSStats M.empty
    step l erc = 
      case erc of
        Left _ -> l
        Right rc ->
          let nst = 
                GSStats (M.singleton (recordsParameters rc) (recordsObservables rc))
           in l <> nst

gsStats :: [Either String GSIO.GSRecord] -> GSStats
gsStats = Fold.fold accumulateRecords

size :: GSStats -> !Int
size (GSStats s) = M.size s
