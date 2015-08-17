{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Eclogues.State.Types where

import Eclogues.JobSpec (Name, JobStatus)

import Control.Lens.TH (makeClassy)
import Data.Default.Generics (Default)
import Data.HashMap.Lazy (HashMap)
import GHC.Generics (Generic)

type Jobs    = HashMap Name JobStatus
type RevDeps = HashMap Name [Name]

data AppState = AppState { _jobs    :: Jobs
                         , _revDeps :: RevDeps }
                deriving (Generic, Show, Eq)

$(makeClassy ''AppState)

instance Default AppState
