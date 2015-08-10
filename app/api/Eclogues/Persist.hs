{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Persistence for job states and yet-to-be-run 'ScheduleCommand's.
-}

module Eclogues.Persist (
                        -- * 'Action'
                          Action, Context
                        -- ** Running
                        , withPersistDir, atomically
                        -- * View
                        , allIntents, allJobs
                        -- * Mutate
                        , insert, updateState, delete, scheduleIntent, deleteIntent )
                        where

import Eclogues.Persist.Stage1 ()
import Eclogues.Scheduling.Command (ScheduleCommand)
import Eclogues.JobSpec (Name, JobSpec, JobStatus (JobStatus))
import qualified Eclogues.JobSpec as Job

import Control.Lens ((^.))
import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.UUID (UUID)
import Database.Persist.TH (mkPersist, sqlSettings, mkMigrate, share, persistLowerCase)
import Database.Persist ((==.), (=.))
import qualified Database.Persist as P
import qualified Database.Persist.Sql as PSql
import Database.Persist.Sqlite (withSqlitePool)

-- Table definitions.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Job
    name Name
    spec JobSpec
    state Job.JobState
    uuid UUID
    UniqueName name
ScheduleIntent
    command ScheduleCommand
    UniqueCommand command
|]

-- Hide away the implementation details.
-- | All 'Action's run in a Context.
newtype Context  = Context PSql.ConnectionPool
-- | An interaction with the persistence backend.
newtype Action r = Action (ReaderT PSql.SqlBackend IO r)
                          deriving (Functor, Applicative, Monad)

-- | You can join up Actions by running them sequentially.
instance Monoid (Action ()) where
    mempty = pure ()
    mappend = (*>)

-- | Run some action that might persist things inside the given directory.
-- Logs to stderr.
withPersistDir :: FilePath -> (Context -> LoggingT IO a) -> IO a
withPersistDir path f = runStderrLoggingT $ withSqlitePool ("WAL=off " <> (T.pack path) <> "/eclogues.db3") 1 act where
    act pool = do
        PSql.runSqlPool (PSql.runMigration migrateAll) pool
        f (Context pool)

-- | Apply some Action in a transaction.
atomically :: Context -> Action r -> IO r
atomically (Context pool) (Action a) = PSql.runSqlPool a pool

insert :: JobStatus -> Action ()
insert (JobStatus spec st uuid) = Action $ P.insert_ job where
    job = Job { jobName = spec ^. Job.name
              , jobSpec = spec
              , jobState = st
              , jobUuid = uuid }

updateState :: Name -> Job.JobState -> Action ()
updateState name st = Action $ P.updateWhere [JobName ==. name] [JobState =. st]

delete :: Name -> Action ()
delete = Action . P.deleteBy . UniqueName

scheduleIntent :: ScheduleCommand -> Action ()
scheduleIntent = Action . P.insert_ . ScheduleIntent

deleteIntent :: ScheduleCommand -> Action ()
deleteIntent = Action . P.deleteBy . UniqueCommand

getAll :: (PSql.SqlBackend ~ PSql.PersistEntityBackend a, PSql.PersistEntity a) => (a -> b) -> Action [b]
getAll f = Action $ fmap (f . P.entityVal) <$> P.selectList [] []

allIntents :: Action [ScheduleCommand]
allIntents = getAll scheduleIntentCommand

allJobs :: Action [JobStatus]
allJobs = getAll toStatus where
    toStatus (Job _ spec st uuid) = JobStatus spec st uuid
