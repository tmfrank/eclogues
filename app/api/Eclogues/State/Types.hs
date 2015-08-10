{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable
-}

module Eclogues.State.Types where

import Eclogues.JobSpec (Name, JobStatus)

import Control.Lens.TH (makeClassy)
import Data.Default.Generics (Default)
import Data.HashMap.Lazy (HashMap)
import GHC.Generics (Generic)

-- | Map of jobs names to status.
type Jobs    = HashMap Name JobStatus
-- | Map of job names to jobs that depend on them.
type RevDeps = HashMap Name [Name]

data AppState = AppState { _jobs    :: Jobs
                         , _revDeps :: RevDeps }
                deriving (Generic, Show, Eq)

$(makeClassy ''AppState)

instance Default AppState
