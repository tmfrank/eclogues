{-# LANGUAGE TemplateHaskell #-}
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

import Control.Exception (displayException)
import Data.Aeson (FromJSON (..), eitherDecode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (readFile)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Path (Path, Abs, Rel, Dir, stripDir, mkAbsDir, parseAbsDir)

newtype AbsDir = AbsDir { getDir :: Path Abs Dir }

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

toRelPath :: Path Abs a -> Path Rel a
toRelPath = either (error "abs path is not abs") id . stripDir $(mkAbsDir "/")

instance FromJSON AbsDir where
    parseJSON (Aeson.String s) = toP $ parseAbsDir $ T.unpack s
      where
        toP = either (fail . ("Absolute dir: " ++) . displayException) (pure . AbsDir)
    parseJSON _                = fail "Absolute dir must be string"

bool :: a -> a -> Bool -> a
bool a b p = if p then b else a
