module Eclogues.Util where

import Prelude hiding (readFile)

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (readFile)
import Data.Maybe (fromMaybe)

readJSON :: (FromJSON a) => FilePath -> IO (Either String a)
readJSON = fmap eitherDecode . readFile

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
