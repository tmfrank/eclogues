{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eclogues.State.Monad where

import Eclogues.Scheduling.Command (ScheduleCommand (..))
import Eclogues.State.Types (AppState (..), jobs, revDeps)
import Eclogues.TaskSpec (Name, TaskSpec (..), JobState, JobStatus (JobStatus), jobState, taskName)

import Control.Applicative (Applicative, pure)
import Control.Lens ((%~), (?=), (%=), (^.), at, ix, sans, use, non)
import Control.Monad (void)
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List

newtype EStateT m a = EStateT { unEStateT :: WriterT [ScheduleCommand] (StateT AppState m) a }
                      deriving (Functor, Applicative, Monad)

instance MonadTrans EStateT where
    lift = EStateT . lift . lift

instance MFunctor EStateT where
    hoist nat (EStateT m) = EStateT $ (hoist $ hoist nat) m

type EState = EStateT Identity

runEStateT :: (Applicative m, Monad m) => AppState -> EStateT m a -> m (a, AppState, [ScheduleCommand])
runEStateT i (EStateT m) = do
    ((a, w), s) <- flip runStateT i $ runWriterT m
    pure (a, s, w)

runEState :: AppState -> EState a -> (a, AppState, [ScheduleCommand])
runEState st = runIdentity . runEStateT st

schedule :: (Monad m) => ScheduleCommand -> EStateT m ()
schedule = EStateT . tell . (:[])

insertJob :: (Monad m) => TaskSpec -> JobState -> EStateT m ()
insertJob spec st = EStateT . lift $ jobs . at (spec ^. taskName) ?= JobStatus spec st

deleteJob :: (Monad m) => Name -> EStateT m ()
deleteJob name = EStateT . lift $ jobs %= sans name

getJob :: (Functor m, Monad m) => Name -> EStateT m (Maybe JobStatus)
getJob name = EStateT . lift . use $ jobs . at name

modifyJobState :: (Monad m) => (JobState -> JobState) -> Name -> EStateT m ()
modifyJobState f name = EStateT . lift $ jobs . ix name %= (jobState %~ f)

setJobState :: (Functor m, Monad m) => Name -> JobState -> EStateT m ()
setJobState name = void . flip modifyJobState name . const

addRevDep :: (Monad m) => Name -> Name -> EStateT m ()
addRevDep on by = EStateT . lift $ revDeps %= HashMap.insertWith (++) on [by]

removeRevDep :: (Monad m) => Name -> Name -> EStateT m ()
removeRevDep on by = EStateT . lift $ revDeps . at on %= (>>= removed) where
    removed lst = case List.delete by lst of
        [] -> Nothing
        a  -> Just a

getDependents :: (Functor m, Monad m) => Name -> EStateT m [Name]
getDependents name = EStateT . lift . use $ revDeps . at name . non []
