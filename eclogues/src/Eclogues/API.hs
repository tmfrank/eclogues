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

Eclogues REST API definition. See also the <https://github.com/rimmington/eclogues#api README>.
-}

module Eclogues.API (
      API, JobError (..), AbsFile, Get'
    -- * Health reporting
    , Health, mkHealth, schedulerAccessible
    ) where

import qualified Eclogues.Job as Job
import Eclogues.ServantInstances ()

import Control.Lens (Lens', (&), (.~))
import Control.Lens.TH (makeLensesWith, lensRules, generateSignatures)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Network.URI (URI)
import Path (Path, Abs, File)
import Servant.API ((:>), (:<|>), Get, Post, Put, Delete, ReqBody, Capture, QueryParam, JSON)

-- | An absolute 'Path' to a file.
type AbsFile = Path Abs File
type Get' = Get '[JSON]

-- NB: Make sure the module header is updated with this.
-- | Eclogues API definition.
type API =  "jobs"   :> Get' [Job.Status]
       :<|> "jobs"   :> Capture "name" Job.Name  :> Get' Job.Status
       :<|> "jobs"   :> Capture "name" Job.Name  :> "stage" :> Get' Job.Stage
       :<|> "jobs"   :> Capture "name" Job.Name  :> "stage" :> ReqBody '[JSON] Job.Stage  :> Put '[JSON] ()
       :<|> "jobs"   :> Capture "name" Job.Name  :> "scheduler" :> Get' ()
       :<|> "jobs"   :> Capture "name" Job.Name  :> "output" :> QueryParam "path" AbsFile :> Get' ()
       :<|> "jobs"   :> Capture "name" Job.Name  :> Delete '[JSON] ()
       :<|> "jobs"   :> ReqBody '[JSON] Job.Spec :> Post '[JSON] ()
       :<|> "health" :> Get' Health

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
