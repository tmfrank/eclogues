{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Units where

import Data.Proxy (Proxy (Proxy))
import Data.Void (Void)

data Byte
data Mega u
data Mebi u
data Core

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

instance (UnitPrefix p, Unit a) => Unit (p a) where
    unitString = const $ prefixString (Proxy :: Proxy (p Void)) ++ unitString (Proxy :: Proxy a)

instance UnitPrefix Mega where
    multiplier = const 1000000
    prefixString = const "M"

instance UnitPrefix Mebi where
    multiplier = const 1048576
    prefixString = const "Mi"

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

byte :: a -> Value a Byte
byte = Value

core :: a -> Value a Core
core = Value

mega :: (Fractional a) => (a -> Value a u) -> a -> Value a (Mega u)
mega f = Value

mebi :: (Fractional a) => (a -> Value a u) -> a -> Value a (Mebi u)
mebi f = Value
