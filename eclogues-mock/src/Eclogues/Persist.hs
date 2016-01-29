{-# LANGUAGE FlexibleContexts #-}
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
    , insert, updateStage, updateSatis, delete, scheduleIntent, deleteIntent
    ) where

import Eclogues.Persist.Stage1 ()
import Eclogues.Scheduling.Command (ScheduleCommand)
import qualified Eclogues.Job as Job

import Control.Lens ((^.))
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Logger (LoggingT, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.UUID (UUID)
import Database.Persist.TH (mkPersist, sqlSettings, mkMigrate, share, persistLowerCase)
import Database.Persist ((==.), (=.))
import qualified Database.Persist as P
import qualified Database.Persist.Sql as PSql
import Database.Persist.Sqlite (withSqlitePool)
import Path (Path, Abs, Dir, toFilePath)

-- Table definitions.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Job
    name Job.Name
    spec Job.Spec
    stage Job.Stage
    satis Job.Satisfiability
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
withPersistDir :: Path Abs Dir -> (Context -> LoggingT IO a) -> IO a
withPersistDir path f = runStderrLoggingT $ withSqlitePool ("WAL=off " <> path' <> "/eclogues.db3") 1 act
  where
    act pool = do
        PSql.runSqlPool (PSql.runMigration migrateAll) pool
        f (Context pool)
    path' = T.pack $ toFilePath path

-- | Apply some Action in a transaction.
atomically :: (MonadBase IO m) => Context -> Action r -> m r
atomically (Context pool) (Action a) = liftBase $ PSql.runSqlPool a pool

insert :: Job.Status -> Action ()
insert status = Action $ P.insert_ job where
    job = Job { jobName  = status ^. Job.name
              , jobSpec  = status ^. Job.spec
              , jobStage = status ^. Job.stage
              , jobSatis = status ^. Job.satis
              , jobUuid  = status ^. Job.uuid }

updateStage :: Job.Name -> Job.Stage -> Action ()
updateStage name st = Action $ P.updateWhere [JobName ==. name] [JobStage =. st]

updateSatis :: Job.Name -> Job.Satisfiability -> Action ()
updateSatis name st = Action $ P.updateWhere [JobName ==. name] [JobSatis =. st]

delete :: Job.Name -> Action ()
delete = Action . P.deleteBy . UniqueName

scheduleIntent :: ScheduleCommand -> Action ()
scheduleIntent = Action . P.insert_ . ScheduleIntent

deleteIntent :: ScheduleCommand -> Action ()
deleteIntent = Action . P.deleteBy . UniqueCommand

getAll :: (PSql.SqlBackend ~ PSql.PersistEntityBackend a, PSql.PersistEntity a) => (a -> b) -> Action [b]
getAll f = Action $ fmap (f . P.entityVal) <$> P.selectList [] []

allIntents :: Action [ScheduleCommand]
allIntents = getAll scheduleIntentCommand

allJobs :: Action [Job.Status]
allJobs = getAll toStatus where
    toStatus (Job _ spec st satis uuid) = Job.mkStatus spec st satis uuid
