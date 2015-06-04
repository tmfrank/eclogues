{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Eclogues.State.Monad where

import qualified Eclogues.Persist as Persist
import Eclogues.Scheduling.Command (ScheduleCommand (..))
import Eclogues.State.Types (AppState (..), jobs, revDeps)
import qualified Eclogues.State.Types as EST
import Eclogues.TaskSpec (Name, TaskSpec (..), JobState, JobStatus (JobStatus), jobState, taskName)

import Control.Applicative (Applicative)
import Control.Lens ((%~), (.~), (?=), (%=), (^.), (<>=), at, ix, sans, use, non)
import Control.Lens.TH (makeClassy)
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List

data TransitionaryState = TransitionaryState { _appState :: AppState
                                             , _scheduleCommands :: [ScheduleCommand]
                                             , _persist  :: Maybe (Persist.PersistAction ()) }

$(makeClassy ''TransitionaryState)
instance EST.HasAppState TransitionaryState where appState = appState

newtype EStateT m a = EStateT { unEStateT :: StateT TransitionaryState m a }
                      deriving (Functor, Applicative, Monad)

instance MonadTrans EStateT where
    lift = EStateT . lift

instance MFunctor EStateT where
    hoist nat (EStateT m) = EStateT $ (hoist nat) m

type EState = EStateT Identity

runEStateT :: (Applicative m, Monad m) => AppState -> EStateT m a -> m (a, TransitionaryState)
runEStateT i (EStateT m) = runStateT m $ TransitionaryState i [] Nothing

runEState :: AppState -> EState a -> (a, TransitionaryState)
runEState st = runIdentity . runEStateT st

schedule :: (Monad m) => ScheduleCommand -> EStateT m ()
schedule cmd = EStateT $ do
    scheduleCommands %= (cmd :)
    persist <>= Just (Persist.scheduleIntent cmd)

insertJob :: (Monad m) => TaskSpec -> JobState -> EStateT m ()
insertJob spec st = EStateT $ do
    let status = JobStatus spec st
    jobs . at (spec ^. taskName) ?= status
    persist <>= Just (Persist.insert status)

deleteJob :: (Monad m) => Name -> EStateT m ()
deleteJob name = EStateT $ do
    jobs %= sans name
    persist <>= Just (Persist.delete name)

getJob :: (Functor m, Monad m) => Name -> EStateT m (Maybe JobStatus)
getJob name = EStateT . use $ jobs . at name

modifyJobState :: (Monad m) => (JobState -> JobState) -> Name -> EStateT m ()
modifyJobState f name = EStateT $ jobs . ix name %= (jobState %~ f)

setJobState :: (Functor m, Monad m) => Name -> JobState -> EStateT m ()
setJobState name st = EStateT $ do
    jobs . ix name %= (jobState .~ st)
    persist <>= Just (Persist.updateState name st)

addRevDep :: (Monad m) => Name -> Name -> EStateT m ()
addRevDep on by = EStateT $ revDeps %= HashMap.insertWith (++) on [by]

removeRevDep :: (Monad m) => Name -> Name -> EStateT m ()
removeRevDep on by = EStateT $ revDeps . at on %= (>>= removed) where
    removed lst = case List.delete by lst of
        [] -> Nothing
        a  -> Just a

getDependents :: (Functor m, Monad m) => Name -> EStateT m [Name]
getDependents name = EStateT . use $ revDeps . at name . non []
