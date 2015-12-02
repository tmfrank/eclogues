{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Template Haskell stage restricted definitions and orphan instances for
"Eclogues.Persist".
-}

module Eclogues.Persist.Stage1 where

import Eclogues.Scheduling.Command (ScheduleCommand)
import qualified Eclogues.Job as Job

import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecodeStrict)
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Combinators (mapLeft)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.UUID (UUID, toASCIIBytes, fromASCIIBytes)
import qualified Database.Persist as P
import qualified Database.Persist.Sql as PSql

jsonPersistValue :: (ToJSON a) => a -> P.PersistValue
jsonPersistValue = P.PersistByteString . BSL.toStrict . encode

jsonPersistParse :: (FromJSON a) => T.Text -> P.PersistValue -> Either T.Text a
jsonPersistParse _ (P.PersistByteString bs) = mapLeft T.pack $ eitherDecodeStrict bs
jsonPersistParse n e                        = Left . (("Invalid " <> n <> " persist value ") <>) . T.pack $ show e

instance P.PersistField Job.Stage where
    toPersistValue = jsonPersistValue
    fromPersistValue = jsonPersistParse "Stage"

instance PSql.PersistFieldSql Job.Stage where
    sqlType _ = PSql.SqlBlob

instance P.PersistField Job.Satisfiability where
    toPersistValue = jsonPersistValue
    fromPersistValue = jsonPersistParse "Job.Satisfiability"

instance PSql.PersistFieldSql Job.Satisfiability where
    sqlType _ = PSql.SqlBlob

instance P.PersistField Job.Spec where
    toPersistValue = jsonPersistValue
    fromPersistValue = jsonPersistParse "Spec"

instance PSql.PersistFieldSql Job.Spec where
    sqlType _ = PSql.SqlBlob

instance P.PersistField ScheduleCommand where
    toPersistValue = jsonPersistValue
    fromPersistValue = jsonPersistParse "ScheduleCommand"

instance PSql.PersistFieldSql ScheduleCommand where
    sqlType _ = PSql.SqlBlob

instance P.PersistField UUID where
    toPersistValue = P.PersistByteString . toASCIIBytes
    fromPersistValue (P.PersistByteString bs)
        | Just res <- fromASCIIBytes bs = Right res
        | otherwise                     = Left "Invalid UUID persist bytestring"
    fromPersistValue _                  = Left "Invalid UUID persist value"

instance PSql.PersistFieldSql UUID where
    sqlType _ = PSql.SqlBlob

instance P.PersistField Job.Name where
    toPersistValue = P.toPersistValue . Job.nameText
    fromPersistValue = mapLeft T.pack . Job.mkName <=< P.fromPersistValue

instance PSql.PersistFieldSql Job.Name where
    sqlType _ = PSql.sqlType (Proxy :: Proxy T.Text)
