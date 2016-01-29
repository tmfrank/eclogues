{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Representation of communication with the remote scheduler.
-}

module Eclogues.Scheduling.Command (ScheduleCommand (..)) where

import qualified Eclogues.Job as Job

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.UUID (UUID)

-- | Tell the scheduler to do something.
data ScheduleCommand = QueueJob   Job.Spec UUID
                     | KillJob    Job.Name UUID
                     | CleanupJob Job.Name UUID

$(deriveJSON defaultOptions ''ScheduleCommand)
