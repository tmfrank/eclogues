{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Utility definitions.
-}

module Eclogues.Util where

import Prelude hiding (readFile)

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (readFile)
import Data.Maybe (fromMaybe)

readJSON :: (FromJSON a) => FilePath -> IO (Either String a)
readJSON = fmap eitherDecode . readFile

-- | 'error' on 'Left'.
orError :: Either String a -> IO a
orError = \case
    Right a -> pure a
    Left  e -> error e

orShowError :: (Show e) => Either e a -> IO a
orShowError = \case
    Right a -> pure a
    Left e  -> error $ show e

maybeDo :: (Applicative f) => Maybe (f ()) -> f ()
maybeDo = fromMaybe $ pure ()
