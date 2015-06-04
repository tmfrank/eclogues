{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Eclogues.State.Types where

import Eclogues.TaskSpec (Name, JobStatus)

import Control.Lens.TH (makeClassy)
import Data.HashMap.Lazy (HashMap, empty)

type Jobs    = HashMap Name JobStatus
type RevDeps = HashMap Name [Name]

data AppState = AppState { _jobs    :: Jobs
                         , _revDeps :: RevDeps }

$(makeClassy ''AppState)

newAppState :: AppState
newAppState = AppState empty empty
