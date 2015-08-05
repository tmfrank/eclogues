{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Eclogues.API where

import Eclogues.JobSpec (JobStatus, JobState, JobSpec, Name)
import Eclogues.ServantInstances ()

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Network.URI (URI)
import Path (Path, Abs, File)
import Servant.API ((:>), (:<|>), Get, Post, Put, Delete, ReqBody, Capture, QueryParam, JSON)

type AbsFile = Path Abs File
type Get' = Get '[JSON]

type VAPI =  "jobs"   :> Get' [JobStatus]
        :<|> "jobs"   :> Capture "name" Name :> Get' JobStatus
        :<|> "jobs"   :> Capture "name" Name :> "state" :> Get' JobState
        :<|> "jobs"   :> Capture "name" Name :> "state" :> ReqBody '[JSON] JobState :> Put '[JSON] ()
        :<|> "jobs"   :> Capture "name" Name :> "scheduler" :> Get' ()
        :<|> "jobs"   :> Capture "name" Name :> "output" :> QueryParam "path" AbsFile :> Get' ()
        :<|> "jobs"   :> Capture "name" Name :> Delete '[JSON] ()
        :<|> "jobs"   :> ReqBody '[JSON] JobSpec :> Post '[JSON] ()
        :<|> "health" :> Get' Health

data JobError = JobNameUsed
              | NoSuchJob
              | JobMustExist Name
              | JobCannotHaveFailed Name
              | JobMustBeTerminated Bool
              | OutstandingDependants [Name]
              | InvalidStateTransition String
              | SchedulerRedirect URI
              | SchedulerInaccessible
                deriving (Show, Eq)

data Health = Health { schedulerAccessible :: Bool }

$(deriveJSON defaultOptions ''JobError)
$(deriveJSON defaultOptions ''Health)
