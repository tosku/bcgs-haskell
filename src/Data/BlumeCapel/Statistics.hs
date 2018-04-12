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
  , printStats
  , readStats
  , plotStats
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
import Data.Either.Unwrap (fromRight)
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
instance AE.ToJSON GSParams
{-instance AE.ToJSON GSParams where-}
  {-toJSON pars =-}
    {-let fld = fromRational (field pars) :: Double-}
        {-dis = fromRational (disorder_strength pars) :: Double-}
     {-in AE.object [ "linear_size" AE..= linear_size pars-}
                  {-, "dimensions" AE..= dimensions pars-}
                  {-, "field" AE..= fld-}
                  {-, "disorder" AE..= dis-}
                  {-, "disorder_type" AE..= disorder_type pars-}
                  {-]-}

-- | Numerical stable aggregator for variance
data Variance a = Variance !Int !a !a
  deriving (Show, Eq, GEN.Generic)
instance Fractional a => Monoid (Variance a) where
  mempty = Variance 0 0 0
  mappend (Variance count mean m2) (Variance 1 newValue _) =
    Variance count' mean' m2'
      where count' = count + 1
            n' = fromIntegral count'
            delta = newValue - mean
            mean' = mean + delta / n'
            delta2 = newValue - mean'
            m2' = m2 + delta * delta2
  mappend (Variance count_a avg_a m2a) (Variance count_b avg_b m2b) =
    Variance count' mean' m2'
      where count' = count_a + count_b
            count_a' = fromIntegral count_a
            count_b' = fromIntegral count_b
            delta = avg_b - avg_a
            mean' = (avg_a * count_a' + avg_b * count_b') / (count_a' + count_b')
            m2' = m2a + m2b + delta^2 * count_a' * count_b' / (count_a' + count_b')

getMean :: Variance Double -> Double
getMean (Variance _ m _) = m

getVariance :: Variance Double -> Stats
getVariance (Variance n x x2) =
  if n < 2 
    then 
      Stats { n = n
            , average = x
            , var = Left n
            }
    else
      Stats { n = n
            , average = x
            , var = Right $ x2 / (fromIntegral n - 1)
            }

-- | Container for computing Covariance
data Covariance a = Covariance !Int !a !a !a
  deriving (Show, Eq, GEN.Generic)
instance Fractional a => Monoid (Covariance a) where
  mempty = Covariance 0 0 0 0
  -- | https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
  mappend (Covariance na meanxa meanya cva) (Covariance nb meanxb meanyb cvb) =
    Covariance n meanx meany cv
      where n = na + nb
            n' = fromIntegral n
            na' = fromIntegral na
            nb' = fromIntegral nb
            !meanx = (meanxa * na' + meanxb * nb') / n'
            !meany = (meanya * na' + meanyb * nb') / n'
            !cv = cva + cvb + (meanxa - meanxb) * (meanya - meanyb) * (na' * nb' /n')

getCovariance :: Covariance Double -> Double
getCovariance (Covariance n meanx meany cc) =
  let n' = fromIntegral n
   in cc / (n' - 1)

data ACC = ACC { mag  :: Variance Double
               , mag2 :: Variance Double
               , mag4 :: Variance Double
               , bcum :: Covariance Double
               , zcl2 :: Variance Double
               , ener :: Variance Double
               }
  deriving (Show, Eq, GEN.Generic)
instance Monoid ACC where
  mempty = ACC { mag = Variance 0 0 0
               , mag2 = Variance 0 0 0
               , mag4 = Variance 0 0 0
               , bcum = Covariance 0 0 0 0
               , zcl2 = Variance 0 0 0
               , ener = Variance 0 0 0
               }
  mappend l r = ACC { mag = mag l <> mag r
                    , mag2 = mag2 l <> mag2 r
                    , mag4 = mag4 l <> mag4 r
                    , bcum = bcum l <> bcum r
                    , zcl2 = zcl2 l <> zcl2 r
                    , ener = ener l <> ener r
                    }

newtype GSACC = GSACC (M.Map GSParams ACC)
  deriving (Show, Eq, GEN.Generic)
instance Monoid GSACC where
  mempty = GSACC M.empty
  mappend (GSACC l) (GSACC r) = GSACC $
    M.unionWith (<>) l r

data Stats = Stats { n       :: !Int
                   , average :: !Double
                   , var  :: !(Either Int Double)
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
  let !pars = recordsParameters rc
      !m = GSIO.magnetization $ GSIO.observables rc
      !zrs = IM.toList $ GSIO.zeroclusters $ GSIO.observables rc
      -- | lattice size
      !nn = (linear_size pars)^(dimensions pars)
      !numclusters = length zrs
      !l2 = if numclusters == 0
              then 0
              else (foldl'
                (\ac (s, n) -> ac + (fromIntegral n) * ((fromIntegral s)^2))
                0 zrs) / ((fromIntegral nn)^2) / (fromIntegral numclusters)
      !l4 = if numclusters == 0
              then 0
              else (foldl'
                (\ac (s, n) -> ac + (fromIntegral n) * ((fromIntegral s)^4))
                0 zrs) / ((fromIntegral nn)^4) / (fromIntegral numclusters)
      !en = GSIO.energy $ GSIO.observables rc
   in GSACC (M.singleton pars (ACC { mag = Variance 1 m 0
                                   , mag2 = Variance 1 (m^2) 0
                                   , mag4 = Variance 1 (m^4) 0
                                   , bcum = Covariance 1 (m^2) (m^4) 0
                                   , zcl2 = Variance 1 l2 0
                                   , ener = Variance 1 en 0
                                   }))


stdErr :: Stats -> Either Int Double
stdErr v = 
  case var v of
    Left n'  -> Left n'
    Right vr -> Right $ sqrt (vr / (fromIntegral (n v)))

getBinderCumulant :: Variance Double -> Variance Double -> Covariance Double -> Stats
getBinderCumulant (Variance na x2 d2) (Variance nb x4 d4) cov24 =
  let var2 =  case (var (getVariance (Variance na x2 d2))) of
                Left _ -> -1
                Right var2 -> var2
      var4 =  case (var $ getVariance (Variance nb x4 d4)) of
                Left _ -> -1
                Right var4 -> var4 
      cov = getCovariance cov24
-- | $ \frac{\partial b}{\partial m_2} $
{-
        2⋅m₄
m2' = ─────
          3
      3⋅m₂
-}
      tbx2 = (2*x4) / (3 * (x2^2))
-- | $ \frac{\partial b}{\partial m_4} $ 
{-
      -1
m4'=  ─────
        2
    3⋅m₂
-}
      tbx4 = (-1) / (3*(x2^2))
      var' = (abs tbx2)^2 * var2 + (abs tbx4)^2 * var4 + 2 * tbx2 * tbx4 * cov
   in Stats { n = na
            , average = 1 - (x4)/(3 *(x2^2))
            , var = if var2 == -1 || var4 == -1
                          then 
                            Left na
                          else 
                            Right var'
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
            m4 = mag4 x
            bc = bcum x
            l2 = zcl2 x
         in GSStats (M.insert par (Observables { magnetization = getVariance m
                                               , binderCumulant = getBinderCumulant m2 m4 bc
                                               , l2 = getVariance l2
                                               }) gsac))
         (GSStats M.empty) gaccu

gsStats :: [Either String GSIO.GSRecord] -> GSStats
gsStats = Fold.fold accumulateRecords

size :: GSStats -> Int
size (GSStats s) = M.size s

keys :: GSStats -> [GSParams]
keys (GSStats s) = M.keys s

toList :: GSStats -> [(GSParams,Observables)]
toList (GSStats s) = M.toList s

printStats :: GSStats -> String -> IO ()
printStats stats outfile = do
  I.writeFile outfile
    $ encodeToLazyText stats
  putStrLn $ "printed summed json in " ++ outfile ++ "\n"

readStats :: String -> IO (Either String GSStats)
readStats sumfilename = do
  bytes <- B.readFile sumfilename
  return $ AE.eitherDecode $ bytes

rparse :: AE.ToJSON a => a -> String
rparse = TXL.unpack . encodeToLazyText


plotStats :: String -> String -> IO ()
plotStats statsfile fieldname = do
  stats <- fmap fromRight $ readStats statsfile
  let numberofpoints = size stats
      label = mconcat $ map encodeToLazyText (keys stats)
      field' = case fieldname of
                "mag" -> magnetization
                "binder" -> binderCumulant
                "l2" -> l2
                otherwise -> magnetization
      xs = rparse (fmap (fromRational . field) $ keys stats :: [Double])
      values = rparse (fmap (\(_,obs) -> 
                                let x = field' obs
                                    m = average x
                                    se = fromRight $ stdErr x
                                 in (m,se)
                             ) $ toList stats :: [(Double,Double)])
  {-putStrLn $ GSIO.getJson label-}
  R.withEmbeddedR R.defaultConfig $ do
    R.runRegion $ do
      [HR.r| require(ggplot2)
             require(rjson)
             values = fromJSON(values_hs)
             xs = fromJSON(xs_hs)
             print(values)
             valuesList = t(matrix(unlist(lapply(values, function(x){
                 de = x[[2]]
                 out = c(x[[1]],x[[1]] - (1.96 * de),x[[1]] + (1.96 * de))
                 return(out)
             })),ncol=length(xs),nrow=3))
             colnames(valuesList)=c("mean","ymin","ymax")
             print("valuesList")
             print(valuesList)
             means = valuesList[,"mean"]
             lows = valuesList[,"ymin"]
             highs = valuesList[,"ymax"]
             print("means")
             print(means)
             fvm = data.frame(xs,means,lows,highs)
             print("fvm")
             print(fvm)
             theplot = ggplot(fvm,aes(x=xs,y=means)) +
             geom_point() +
             geom_errorbar(aes(ymin=lows, ymax=highs),width=.001)
             ggsave(filename="fieldvsValues.svg",plot=theplot)
        |]
      return ()
