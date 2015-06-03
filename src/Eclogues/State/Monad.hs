{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eclogues.State.Monad where

import Eclogues.Scheduling.Command (ScheduleCommand (..))
import Eclogues.State.Types (AppState (..), withJobs, withRevDeps)
import Eclogues.TaskSpec (Name, TaskSpec (..), JobState, JobStatus (..))

import Control.Applicative (Applicative, (<$>), pure)
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, runStateT, modify, get)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Lazy as HashMap

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
insertJob spec st = EStateT . lift . modify . withJobs . HashMap.insert (taskName spec) $ JobStatus spec st

deleteJob :: (Monad m) => Name -> EStateT m ()
deleteJob = EStateT . lift . modify . withJobs . HashMap.delete

getJob :: (Functor m, Monad m) => Name -> EStateT m (Maybe JobStatus)
getJob name = EStateT . lift $ HashMap.lookup name . jobs <$> get

addRevDep :: (Monad m) => Name -> Name -> EStateT m ()
addRevDep on by = EStateT . lift . modify . withRevDeps $ HashMap.insertWith (++) on [by]

getDependents :: (Functor m, Monad m) => Name -> EStateT m [Name]
getDependents name = EStateT . lift $ fromMaybe [] . HashMap.lookup name . revDeps <$> get
