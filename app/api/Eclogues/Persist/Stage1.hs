{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eclogues.Persist.Stage1 where

import Eclogues.Scheduling.Command (ScheduleCommand)
import Eclogues.JobSpec (JobSpec, JobState)

import Data.Aeson (FromJSON, ToJSON, encode, eitherDecodeStrict)
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Combinators (mapLeft)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.UUID (UUID, toASCIIBytes, fromASCIIBytes)
import qualified Database.Persist as P
import qualified Database.Persist.Sql as PSql

jsonPersistValue :: (ToJSON a) => a -> P.PersistValue
jsonPersistValue = P.PersistByteString . BSL.toStrict . encode

jsonPersistParse :: (FromJSON a) => T.Text -> P.PersistValue -> Either T.Text a
jsonPersistParse _ (P.PersistByteString bs) = mapLeft T.pack $ eitherDecodeStrict bs
jsonPersistParse n e                        = Left . (("Invalid " <> n <> " persist value ") <>) . T.pack $ show e

instance P.PersistField JobState where
    toPersistValue = jsonPersistValue
    fromPersistValue = jsonPersistParse "JobState"

instance PSql.PersistFieldSql JobState where
    sqlType _ = PSql.SqlBlob

instance P.PersistField JobSpec where
    toPersistValue = jsonPersistValue
    fromPersistValue = jsonPersistParse "JobSpec"

instance PSql.PersistFieldSql JobSpec where
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
