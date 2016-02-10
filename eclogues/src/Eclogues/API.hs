{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Eclogues REST API definition. See also the <https://github.com/rimmington/eclogues#api README>.
-}

module Eclogues.API (
      API, JobError (..), zkNode, parseZKData
    , AbsFile, Get'
    -- * Specific endpoints
    , ListJobs, JobStatus, JobStage, PutJobStage, JobOnScheduler, JobOutput
    , DeleteJob, CreateJob, GetHealth
    -- * Health reporting
    , Health, mkHealth, schedulerAccessible
    ) where

import qualified Eclogues.Job as Job
import Eclogues.ServantInstances ()

import Data.Aeson (decodeStrict')
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Word (Word16)
import Lens.Micro (Lens', (&), (.~))
import Lens.Micro.TH (makeLensesWith, lensRules, generateSignatures)
import Network.URI (URI)
import Path (Path, Abs, File)
import Servant.API ((:>), (:<|>), Get, Post, Put, Delete, ReqBody, Capture, QueryParam, JSON)

-- | An absolute 'Path' to a file.
type AbsFile = Path Abs File
type Get' = Get '[JSON]

type ListJobs       = "jobs"   :> Get' [Job.Status]
type JobStatus      = "jobs"   :> Capture "name" Job.Name  :> Get' Job.Status
type JobStage       = "jobs"   :> Capture "name" Job.Name  :> "stage" :> Get' Job.Stage
type PutJobStage    = "jobs"   :> Capture "name" Job.Name  :> "stage" :> ReqBody '[JSON] Job.Stage  :> Put '[JSON] ()
type JobOnScheduler = "jobs"   :> Capture "name" Job.Name  :> "scheduler" :> Get' ()
type JobOutput      = "jobs"   :> Capture "name" Job.Name  :> "output" :> QueryParam "path" AbsFile :> Get' ()
type DeleteJob      = "jobs"   :> Capture "name" Job.Name  :> Delete '[JSON] ()
type CreateJob      = "jobs"   :> ReqBody '[JSON] Job.Spec :> Post '[JSON] ()
type GetHealth      = "health" :> Get' Health

-- NB: Make sure the module header is updated with this.
-- | Eclogues API definition.
type API = ListJobs       :<|> JobStatus :<|> JobStage  :<|> PutJobStage
      :<|> JobOnScheduler :<|> JobOutput :<|> DeleteJob :<|> CreateJob
      :<|> GetHealth

-- | The Zookeeper node on which Eclogues instances run elections.
zkNode :: String
zkNode = "/eclogues"

-- | Attempt to parse a host and port from a Zookeeper node involved in Eclogues
-- elections.
parseZKData :: ByteString -> Maybe (String, Word16)
parseZKData = decodeStrict'

-- | The various ways requests can fail.
data JobError =
      JobNameUsed
    -- | Target job does not exist.
    | NoSuchJob
    -- | A referenced job does not exist.
    | JobMustExist Job.Name
    | JobCannotHaveFailed Job.Name
    | JobMustBeTerminated Bool
    -- | A job cannot be deleted because some dependants have not terminated.
    | OutstandingDependants [Job.Name]
    -- | Cannot transition from the current stage to the requested stage.
    | InvalidStageTransition String
    -- | Not actually an error: used to redirect to a scheduler page.
    | SchedulerRedirect URI
    -- | Cannot contact the scheduler.
    | SchedulerInaccessible
    deriving (Show, Eq)

-- | Datatype for health reporting.
data Health = Health { _schedulerAccessible :: Bool }
$(makeLensesWith (lensRules & generateSignatures .~ False) ''Health)

-- | Whether Eclogues can access the scheduler.
schedulerAccessible :: Lens' Health Bool

mkHealth :: Bool  -- ^ 'schedulerAccessible'
         -> Health
mkHealth = Health

$(deriveJSON defaultOptions ''JobError)
$(deriveJSON defaultOptions ''Health)
