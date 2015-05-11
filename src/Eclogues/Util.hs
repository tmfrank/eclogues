module Eclogues.Util where

import Prelude hiding (readFile)

import Control.Applicative (pure)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (readFile)

readJSON :: (FromJSON a) => FilePath -> IO (Either String a)
readJSON = fmap eitherDecode . readFile

orError :: Either String a -> IO a
orError = \case
    Right a -> pure a
    Left  e -> error e
