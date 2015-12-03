{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

The 'Resources' type, here to hide the constructor from our own code.
-}

module Eclogues.Job.Resources (
      Resources, HasResources (..), mkResources
    -- ** Conversions
    -- $conversions
    , diskBytes, diskMB, ramBytes, ramMB, cpuCores, timeSeconds
    ) where

import Control.Monad (MonadPlus, join)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.Text as A
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Metrology (zero)
import Data.Metrology.Computing (
      Data, Parallelism, Time, Byte (Byte), Core (Core), (%>), (#>)
    , ceilToUnit, dataP, parallelismP, susQP_)
import Data.Metrology.Show ()
import Data.Metrology.SI (Second (Second), mega, centi, peta)
import Data.Scientific.Suspicious (
    Sustific, toBoundedInteger, toBoundedRealFloat)
import Data.Text (Text, pack)
import qualified Data.Units.SI.Units.Attoparsec.Text as A
import Lens.Micro (Lens', (^.), to)
import Lens.Micro.TH (Getter, makeLenses)

-- | A set of resources required to run a job.
data Resources = Resources { __disk :: Data
                           , __ram  :: Data
                           , __cpu  :: Parallelism
                           , __time :: Time }
                           deriving (Show, Eq)

$(makeLenses ''Resources)

-- | Most of these are 'Getter's to enforce 'mkResources' constraints.
class HasResources c where
    resources :: Lens' c Resources
    disk :: Getter c Data
    disk = resources . _disk
    ram :: Getter c Data
    ram = resources . _ram
    cpu :: Getter c Parallelism
    cpu = resources . _cpu
    time :: Getter c Time
    time = resources . _time

instance HasResources Resources where resources = id

-- | 'Resources' smart constructor. Checks all resource values are positive and
-- under one peta(unit), and 'fail's if not.
mkResources :: (MonadPlus m) => Data -> Data -> Parallelism -> Time -> m Resources
mkResources d r c t = case () of
    _ | tooLarge  -> fail "Resource value too large"
      | nonPos    -> fail "Non-positive resource value"
      | otherwise -> pure $ Resources d' r' c' t'
  where
    tooLarge = not $ d < maxMB
                  && r < maxMB
                  && c < 1 %> peta Core
                  && t < 1 %> peta Second
    nonPos   = d <= zero
            || r <= zero
            || c <= zero
            || t <= zero
    maxMB    = 1 %> peta Byte
    d'       = mc d $ mega Byte
    r'       = mc r $ mega Byte
    c'       = mc c $ centi Core
    t'       = mc t   Second
    mc v u   = if v <= m then m else v `ceilToUnit` u
      where
        m = 1 %> u

-- $conversions
-- The following conversion functions are guaranteed to neither overflow nor
-- underflow, due to constraints enforced by 'mkResources'.

diskBytes   :: (HasResources s) => Getter s Int64
diskMB      :: (HasResources s) => Getter s Int64
ramBytes    :: (HasResources s) => Getter s Int64
ramMB       :: (HasResources s) => Getter s Int64
cpuCores    :: (HasResources s) => Getter s Double
timeSeconds :: (HasResources s) => Getter s Int64

diskBytes   = to $ \s -> forceToInt64  $ s ^. disk #> Byte
diskMB      = to $ \s -> forceToInt64  $ s ^. disk #> mega Byte
ramBytes    = to $ \s -> forceToInt64  $ s ^. ram  #> Byte
ramMB       = to $ \s -> forceToInt64  $ s ^. ram  #> mega Byte
cpuCores    = to $ \s -> forceToDouble $ s ^. cpu  #> Core
timeSeconds = to $ \s -> forceToInt64  $ s ^. time #> Second

forceToInt64 :: Sustific -> Int64
forceToInt64 x = fromMaybe (forceErr x) $ toBoundedInteger x

forceToDouble :: Sustific -> Double
forceToDouble x = either (forceErr x) id $ toBoundedRealFloat x

forceErr :: Sustific -> a
forceErr x = error $ "too-large resources value slipped through: " ++ show x

instance FromJSON Resources where
    parseJSON (Aeson.Object o) = res where
        res = join $ mkResources
            <$> get "disk" dataP
            <*> get "ram"  dataP
            <*> get "cpu"  parallelismP
            <*> get "time" timeP
        get :: Text -> A.Parser a -> Aeson.Parser a
        get t p = Aeson.withText "resource" (either fail pure . parseOnly p) =<< o .: t
        parseOnly = A.parseOnly . (<* A.endOfInput)
        timeP = (%> Second) <$> susQP_ A.secondP
    parseJSON _                = fail "Invalid resources value"

instance ToJSON Resources where
    toJSON (Resources d r c t) = Aeson.object
        [ "disk" .= go d (mega Byte)
        , "ram"  .= go r (mega Byte)
        , "cpu"  .= go c Core
        , "time" .= go t Second
        ]
      where
        go q u = pack $ show (q #> u) ++ ' ':show u
