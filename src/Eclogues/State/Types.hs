module Eclogues.State.Types where

import Eclogues.TaskSpec (Name, JobStatus)

import Data.HashMap.Lazy (HashMap, empty)

type Jobs    = HashMap Name JobStatus
type RevDeps = HashMap Name [Name]

data AppState = AppState { jobs      :: Jobs
                         , revDeps   :: RevDeps }

newAppState :: AppState
newAppState = AppState empty empty

withJobs :: (Jobs -> Jobs) -> AppState -> AppState
withJobs f a = a{jobs = f (jobs a)}

withRevDeps :: (RevDeps -> RevDeps) -> AppState -> AppState
withRevDeps f a = a{revDeps = f (revDeps a)}
