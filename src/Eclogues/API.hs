{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Eclogues.API where

import Eclogues.TaskSpec (JobStatus, JobState, TaskSpec, Name)

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Servant.API ((:>), (:<|>), Get, Post, Put, Delete, ReqBody, Capture, JSON)

type Get' = Get '[JSON]

type VAPI =  "jobs"   :> Get' [JobStatus]
        :<|> "jobs"   :> Capture "name" Name :> Get' JobStatus
        :<|> "jobs"   :> Capture "name" Name :> "state" :> Get' JobState
        :<|> "jobs"   :> Capture "name" Name :> "state" :> ReqBody '[JSON] JobState :> Put '[JSON] ()
        :<|> "jobs"   :> Capture "name" Name :> "scheduler" :> Get' ()
        :<|> "jobs"   :> Capture "name" Name :> Delete '[JSON] ()
        :<|> "jobs"   :> ReqBody '[JSON] TaskSpec  :> Post '[JSON] ()
        :<|> "health" :> Get' Health

data JobError = JobNameUsed
              | NoSuchJob
              | JobMustExist Name
              | JobCannotHaveFailed Name
              | JobMustBeTerminated Bool
              | OutstandingDependants [Name]
              | InvalidStateTransition String
              | SchedulerRedirect String
              | SchedulerInaccessible
                deriving (Show)

data Health = Health { schedulerAccessible :: Bool }

$(deriveJSON defaultOptions ''JobError)
$(deriveJSON defaultOptions ''Health)
