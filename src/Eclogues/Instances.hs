{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eclogues.Instances where

#if !MIN_VERSION_transformers(0,4,0)
import Control.Monad.Morph (MFunctor (hoist))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
#endif
import qualified Data.Text.Lazy as L
import Servant.Common.Text (FromText (..), ToText (..))

instance FromText L.Text where
    fromText = fmap L.fromStrict . fromText

instance ToText L.Text where
    toText = L.toStrict

#if !MIN_VERSION_transformers(0,4,0)
instance MFunctor (ExceptT e) where
    hoist nat m = ExceptT (nat (runExceptT m))
#endif
