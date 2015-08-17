{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Functions for altering 'AppState' with appropriate side effects in a
'TransitionaryState' state monad.
-}

module Eclogues.State.Monad (
                            -- * 'TransitionaryState'
                              TransitionaryState (..)
                            , HasTransitionaryState (..)
                            -- ** In a 'MonadState'
                            , TS
                            , runStateTS
                            , runState
                            -- * View
                            , getJob
                            , getDependents
                            -- * Mutate
                            , insertJob
                            , deleteJob
                            , setJobState
                            , addRevDep
                            , removeRevDep
                            , schedule
                            -- * Load
                            , loadJobs
                            ) where

import qualified Eclogues.Persist as Persist
import Eclogues.Scheduling.Command (ScheduleCommand (..))
import Eclogues.State.Types (AppState (..), jobs, revDeps)
import qualified Eclogues.State.Types as EST
import Eclogues.JobSpec (Name, JobState, JobStatus)
import qualified Eclogues.JobSpec as Job

import Control.Lens ((.~), (?=), (%=), (^.), (<>=), at, ix, sans, use, non)
import Control.Lens.TH (makeClassy)
import Control.Monad.State (MonadState, StateT, runStateT)
import Data.Default.Generics (Default)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import GHC.Generics (Generic)

-- | A new AppState and the side effects associated with transitioning to it.
data TransitionaryState = TransitionaryState { _appState :: AppState
                                             , _scheduleCommands :: [ScheduleCommand]
                                             , _persist  :: Maybe (Persist.Action ()) }
                          deriving (Generic)

instance Default TransitionaryState

$(makeClassy ''TransitionaryState)
instance EST.HasAppState TransitionaryState where appState = appState

-- | Convenience constraint synonym.
type TS m = (MonadState TransitionaryState m)

runStateTS :: (Monad m) => AppState -> StateT TransitionaryState m a -> m (a, TransitionaryState)
runStateTS i m = runStateT m $ TransitionaryState i [] Nothing

runState :: AppState -> StateT TransitionaryState Identity a -> (a, TransitionaryState)
runState st = runIdentity . runStateTS st

schedule :: (TS m) => ScheduleCommand -> m ()
schedule cmd = do
    scheduleCommands %= (cmd :)
    persist <>= Just (Persist.scheduleIntent cmd)

insertJob :: (TS m) => JobStatus -> m ()
insertJob st = do
    jobs . at (st ^. Job.name) ?= st
    persist <>= Just (Persist.insert st)

deleteJob :: (TS m) => Name -> m ()
deleteJob name = do
    jobs %= sans name
    persist <>= Just (Persist.delete name)

getJob :: (TS m) => Name -> m (Maybe JobStatus)
getJob name = use $ jobs . at name

setJobState :: (TS m) => Name -> JobState -> m ()
setJobState name st = do
    jobs . ix name %= (Job.jobState .~ st)
    persist <>= Just (Persist.updateState name st)

-- | Add a reverse dependency; second name depends on first.
addRevDep :: (TS m) => Name -> Name -> m ()
addRevDep on by = revDeps %= HashMap.insertWith (++) on [by]

-- | Remove a reverse dependency; the second name no longer depends on the first.
removeRevDep :: (TS m) => Name -> Name -> m ()
removeRevDep on by = revDeps . at on %= (>>= removed) where
    removed lst = case List.delete by lst of
        [] -> Nothing
        a  -> Just a

getDependents :: (TS m) => Name -> m [Name]
getDependents name = use $ revDeps . at name . non []

loadJobs :: forall m. (TS m) => [JobStatus] -> m ()
loadJobs = mapM_ go where
    go :: JobStatus -> m ()
    go j = do
        let name = j ^. Job.name
        jobs . at name ?= j
        mapM_ (`addRevDep` name) $ j ^. Job.dependsOn
