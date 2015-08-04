{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eclogues.ServantInstances () where

import qualified Data.Text.Lazy as L
import Servant.Common.Text (FromText (..), ToText (..))

instance FromText L.Text where
    fromText = fmap L.fromStrict . fromText

instance ToText L.Text where
    toText = L.toStrict
