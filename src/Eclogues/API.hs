{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Eclogues.API where

import Eclogues.TaskSpec (JobStatus, JobState, TaskSpec, Name)

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Servant.API ((:>), (:<|>), Get, Post, Put, Delete, ReqBody, Capture, JSON)

type Get' = Get '[JSON]

type VAPI =  "jobs"   :> Get' [JobStatus]
        :<|> "job"    :> Capture "id" Name :> Get' JobStatus
        :<|> "job"    :> Capture "id" Name :> "state" :> Get' JobState
        :<|> "job"    :> Capture "id" Name :> "state" :> ReqBody '[JSON] JobState :> Put '[] ()
        :<|> "job"    :> Capture "id" Name :> Delete '[] ()
        :<|> "create" :> ReqBody '[JSON] TaskSpec  :> Post '[] ()

data JobError = JobNameUsed
              | NoSuchJob
              | JobMustExist Name
              | JobCannotHaveFailed Name
              | JobMustBeTerminated Bool
              | OutstandingDependants [Name]
              | InvalidStateTransition String
                deriving (Show)

$(deriveJSON defaultOptions ''JobError)
