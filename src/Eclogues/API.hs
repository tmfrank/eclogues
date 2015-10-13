{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Eclogues REST API definition.
-}

module Eclogues.API where

import qualified Eclogues.Job as Job
import Eclogues.ServantInstances ()

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Network.URI (URI)
import Path (Path, Abs, File)
import Servant.API ((:>), (:<|>), Get, Post, Put, Delete, ReqBody, Capture, QueryParam, JSON)

type AbsFile = Path Abs File
type Get' = Get '[JSON]

type VAPI =  "jobs"   :> Get' [Job.Status]
        :<|> "jobs"   :> Capture "name" Job.Name  :> Get' Job.Status
        :<|> "jobs"   :> Capture "name" Job.Name  :> "state" :> Get' Job.State
        :<|> "jobs"   :> Capture "name" Job.Name  :> "state" :> ReqBody '[JSON] Job.State  :> Put '[JSON] ()
        :<|> "jobs"   :> Capture "name" Job.Name  :> "scheduler" :> Get' ()
        :<|> "jobs"   :> Capture "name" Job.Name  :> "output" :> QueryParam "path" AbsFile :> Get' ()
        :<|> "jobs"   :> Capture "name" Job.Name  :> Delete '[JSON] ()
        :<|> "jobs"   :> ReqBody '[JSON] Job.Spec :> Post '[JSON] ()
        :<|> "health" :> Get' Health

data JobError = JobNameUsed
              | NoSuchJob
              | JobMustExist Job.Name
              | JobCannotHaveFailed Job.Name
              | JobMustBeTerminated Bool
              | OutstandingDependants [Job.Name]
              | InvalidStateTransition String
              | SchedulerRedirect URI
              | SchedulerInaccessible
                deriving (Show, Eq)

data Health = Health { schedulerAccessible :: Bool }

$(deriveJSON defaultOptions ''JobError)
$(deriveJSON defaultOptions ''Health)
