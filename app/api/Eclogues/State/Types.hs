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

import qualified Eclogues.Job as Job

import Control.Lens.TH (makeClassy)
import Data.Default.Generics (Default)
import Data.HashMap.Lazy (HashMap)
import GHC.Generics (Generic)

type Jobs    = HashMap Job.Name Job.Status
type RevDeps = HashMap Job.Name [Job.Name]

data AppState = AppState { -- | Map of jobs names to status.
                           _jobs    :: Jobs
                           -- | Map of job names to jobs that depend on them
                           -- and have yet to terminate.
                         , _revDeps :: RevDeps }
                deriving (Generic, Show, Eq)

$(makeClassy ''AppState)

instance Default AppState
