{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

(More) type-safe unit types.
-}

module Units where

import Data.Aeson (ToJSON (..), FromJSON (..))
import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy (Proxy))
import Data.Ratio ((%))
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Read (readEither)
import Data.Function (on)

data Byte
data Mega u
data Mebi u
data Micro u
data Core
data Second

type MB = Mega Byte
type MiB = Mebi Byte

class UnitPrefix (p :: * -> *) where
    multiplier :: Proxy (p Void) -> Rational
    prefixString :: Proxy (p Void) -> String

class Unit a where
    unitString :: Proxy a -> String

instance Unit Byte where
    unitString = const "b"

instance Unit Core where
    unitString = const "cores"

instance Unit Second where
    unitString = const "s"

instance (UnitPrefix p, Unit a) => Unit (p a) where
    unitString = const $ prefixString (Proxy :: Proxy (p Void)) ++ unitString (Proxy :: Proxy a)

instance UnitPrefix Mega where
    multiplier = const 1000000
    prefixString = const "M"

instance UnitPrefix Mebi where
    multiplier = const 1048576
    prefixString = const "Mi"

instance UnitPrefix Micro where
    multiplier = const $ 1%1000000
    prefixString = const "µ"

class Convertible ua ub where
    conv :: (Fractional a) => Value a ua -> Value a ub
    as :: (Fractional a) => Value a ua -> (a -> Value a ub) -> Value a ub
    as = const . conv
    asVal :: (Fractional a) => Value a ua -> (a -> Value a ub) -> a
    asVal v f = val $ as v f

instance Convertible Core Core where
    conv = id

instance (UnitPrefix p) => Convertible (p a) a where
    conv (Value a) = Value $ a * fromRational (multiplier (Proxy :: Proxy (p Void)))

instance (UnitPrefix p) => Convertible a (p a) where
    conv (Value a) = Value $ a / fromRational (multiplier (Proxy :: Proxy (p Void)))

instance (UnitPrefix pa, UnitPrefix pb) => Convertible (pa u) (pb u) where
    conv (Value a) = Value $ a * fromRational (mulB / mulA) where
        mulA = multiplier (Proxy :: Proxy (pa Void))
        mulB = multiplier (Proxy :: Proxy (pb Void))

newtype Value a u = Value { val :: a } deriving (Eq)

instance (Show a, Unit u) => Show (Value a u) where
    show (Value a) = show a ++ " " ++ unitString (Proxy :: Proxy u)

instance (Show a, Unit u) => ToJSON (Value a u) where
    toJSON = Aeson.String . Text.pack . show . val

instance (Read a, Unit u) => FromJSON (Value a u) where
    parseJSON (Aeson.String t) = either (fail . ("invalid Value: " ++)) (pure . Value) . readEither $ Text.unpack t
    parseJSON _                = fail "Value must be string"

instance Ord (Value Double Core) where
    compare = compare `on` val

instance Ord (Value Double MiB) where
    compare = compare `on` val

instance Ord (Value Double MB) where
    compare = compare `on` val

byte :: a -> Value a Byte
byte = Value

core :: a -> Value a Core
core = Value

second :: a -> Value a Second
second = Value

mega :: (Fractional a) => (a -> Value a u) -> a -> Value a (Mega u)
mega _ = Value

mebi :: (Fractional a) => (a -> Value a u) -> a -> Value a (Mebi u)
mebi _ = Value

micro :: (Fractional a) => (a -> Value a u) -> a -> Value a (Micro u)
micro _ = Value
