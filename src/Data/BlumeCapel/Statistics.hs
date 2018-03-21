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
  , ObservableCollection (..)
  , sumRecords
  , printStats
  , gsmeans
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

data ObservableCollection = ObservableCollection
  { energies       :: [Double]
  , magnetizations :: [Double]
  , zeroclusters   :: [BC.ZeroDistribution]
  } deriving (Show, GEN.Generic)
instance AE.FromJSON ObservableCollection
instance AE.ToJSON ObservableCollection

emptyCollection :: ObservableCollection
emptyCollection = ObservableCollection
  { energies = []
  , magnetizations = []
  , zeroclusters = []
  }

type GSStats = M.Map GSParams ObservableCollection

recordParameters :: GSIO.GSRecord -> GSParams
recordParameters gr = GSParams
  { linear_size = GSIO.linear_size gr
  , dimensions = GSIO.dimensions gr
  , field = GSIO.field gr
  , disorder_strength = GSIO.disorder_strength gr
  , disorder_type = GSIO.disorder_type gr
  }

updateCollection
  :: GSIO.GSRecord
  -> ObservableCollection
  -> ObservableCollection
updateCollection gr col =
  let obs = GSIO.observables gr
      en = GSIO.energy obs
      mag = GSIO.magnetization obs
      zrs = GSIO.zeroclusters obs
   in ObservableCollection { energies = en : (energies col)
                           , magnetizations = mag : (magnetizations col)
                           , zeroclusters = zrs : (zeroclusters col)
                           }

updateStats :: GSIO.GSRecord -> GSStats -> GSStats
updateStats gr stats =
  let params = recordParameters gr
      oldobs = case M.lookup params stats of
                  Nothing  -> emptyCollection
                  Just obs -> obs
   in M.insert params (updateCollection gr oldobs) stats

sumRecords :: [GSIO.GSRecord] -> GSStats
sumRecords gss = foldr
  (\x ac -> updateStats x ac)
  M.empty gss

printStats :: GSStats -> String -> IO ()
printStats stats outfile = do
  I.writeFile outfile
    $ encodeToLazyText stats
  putStrLn $ "printed summed json in " ++ outfile ++ "\n"

rparse :: AE.ToJSON a => a -> String
rparse = TXT.unpack . TXL.toStrict . encodeToLazyText

gsmeans :: GSStats -> IO ()
gsmeans stats = do
  let numberofpoints = M.size stats
      label = mconcat $ map encodeToLazyText (M.keys stats)
      xs = rparse (map (fromRational . field) $ M.keys stats :: [Double])
      mags = rparse (map (magnetizations . snd) $ M.toList stats :: [[Double]])
  R.withEmbeddedR R.defaultConfig $ do
    R.runRegion $ do
      [HR.r| require(ggplot2)
             require(rjson)
             mags = fromJSON(mags_hs)
             xs = fromJSON(xs_hs)
             magslist = lapply(mags, function(x){
                 return(mean_se(x))
             })
             magmeans = unlist(lapply(magslist,function(x){return(x$y)}))
             maglows = unlist(lapply(magslist,function(x){return(x$ymin)}))
             maghighs = unlist(lapply(magslist,function(x){return(x$ymax)}))
             fvm = data.frame(xs,magmeans,maglows,maghighs)
             print(magslist)
             print(fvm)
             magplot = ggplot(fvm,aes(x=xs,y=magmeans)) +
             geom_point() +
             geom_errorbar(aes(ymin=maglows, ymax=maghighs),width=.001)
             ggsave(filename="fieldvsmags.svg",plot=magplot)
        |]
      return ()
