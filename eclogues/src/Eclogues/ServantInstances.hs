{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Orphan instances for use with Servant.
-}

module Eclogues.ServantInstances () where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Text (pack, unpack)
import qualified Data.Text.Lazy as L
import Network.URI (URI, parseAbsoluteURI)
import Path (Path, Abs, File, parseAbsFile)
import Servant.Common.Text (FromText (..), ToText (..))

instance FromText L.Text where
    fromText = fmap L.fromStrict . fromText

instance ToText L.Text where
    toText = L.toStrict

instance FromText (Path Abs File) where
    fromText = parseAbsFile . unpack

instance ToText (Path Abs File) where
    toText = pack . show

instance ToJSON URI where
    toJSON = Aeson.String . pack . show

instance FromJSON URI where
    parseJSON (Aeson.String s)
        | Just u <- parseAbsoluteURI (unpack s) = pure u
        | otherwise                             = fail "not a valid absolute URI"
    parseJSON _                                 = fail "URI should be string"
