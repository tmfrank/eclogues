{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Specification of jobs and the current state of submitted jobs.
-}

module Eclogues.Job (
    -- * Job status
      Status, HasStatus (..), mkStatus
    -- * Job spec
    , Spec, HasSpec (..), mkSpec
    , Name, nameText, mkName, uuidName
    , Command
    , OutputPath (..)
    -- * Resources
    , module Eclogues.Job.Resources
    -- * Job satisfiability
    , Satisfiability (..), UnsatisfiableReason (..), isUnsatisfiable
    -- * Job lifecycle stage
    , Stage (..), QueueStage (..), FailureReason (..), RunErrorReason (..)
    , majorStage, majorStages
    -- ** Predicates
    , isActiveStage, isTerminationStage, isOnScheduler, isPendingStage, isQueueStage
    ) where

import Eclogues.Job.Aeson
import Eclogues.Job.Resources

import Control.DeepSeq (NFData (..))
import Control.Exception (displayException)
import Control.Monad (MonadPlus)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Hashable (Hashable)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.UUID.Types (UUID)
import Data.UUID.Aeson ()
import Lens.Micro (Lens')
import Lens.Micro.TH (makeLenses)
import GHC.Generics (Generic)
import Path (Path, Abs, File, parseAbsFile, toFilePath)
import Servant.Common.Text (FromText (..), ToText (..))
import System.Exit (ExitCode)
import Text.Regex.Cross ((=~), re)

default (T.Text)

-- | A job name.
newtype Name = Name { _nameText :: T.Text } deriving (Eq, Hashable, NFData)

-- | 'Name' as text.
nameText :: Name -> T.Text
nameText = _nameText

instance Show Name where
    show = show . nameText

instance Ord Name where
    compare = comparing _nameText

-- | A bash command.
type Command = T.Text

-- | Newtype wrapper for JSON serialisation purposes.
newtype OutputPath = OutputPath { getOutputPath :: Path Abs File }
                     deriving (Show, Eq, NFData)

-- | The reason a job has 'Failed'.
data FailureReason =
    -- | The user requested the job be killed.
      UserKilled
    -- | The job 'command' exited with a non-zero exit code.
    | NonZeroExitCode Int
    -- | The job exceeded its allocated 'ram'.
    | MemoryExceeded --(Value Double Byte)
    -- | The job exceeded its allocated 'disk'.
    | DiskExceeded --(Value Double Byte)
    -- | The job exceeded its allocated 'time'.
    | TimeExceeded
    -- | A job in 'dependsOn' failed.
    | DependencyFailed Name
    deriving (Show, Eq, Generic)

-- | Details about an internal error.
data RunErrorReason =
    -- | The scheduler reported an unexpected state.
      BadSchedulerTransition
    -- | Eclogues subexecutor failure.
    | SubexecutorFailure
    -- | The scheduler lost the job.
    | SchedulerLost
    deriving (Show, Eq, Generic)

-- | Whether the job is on an internal or the scheduler queue.
data QueueStage = LocalQueue | SchedulerQueue deriving (Show, Eq, Generic)

-- | A stage in the lifecycle of a job.
data Stage =
    -- | Waiting for a node to become available to run.
      Queued QueueStage
    -- | Waiting for @n@ jobs to be 'Finished'.
    | Waiting Integer
    | Running
    -- | In the process of being killed. Transitions to 'Failed' 'UserKilled'.
    | Killing
    | Finished
    | Failed FailureReason
    -- | An internal error occurred.
    | RunError RunErrorReason
    deriving (Show, Eq, Generic)

-- | Whether it's possible to run a job given known constraints.
data Satisfiability = Satisfiable
                    | Unsatisfiable UnsatisfiableReason
                    -- | There is insufficient information to determine if the
                    -- job is satisfiable.
                    | SatisfiabilityUnknown
                      deriving (Show, Eq, Generic)

-- | The reason a job was determined to be 'Unsatisfiable'.
data UnsatisfiableReason
    -- | There is no node in the cluster with enough resources to satisfy the
    -- job's requested 'resources'.
    = InsufficientResources
    -- ^ One or more dependencies ('dependsOn') are 'Unsatisfiable'.
    | DependenciesUnsatisfiable [Name]
    deriving (Show, Eq, Generic)

-- | Description of a job to run.
data Spec = Spec { __name          :: Name
                 , __command       :: Command
                 , __resources     :: Resources
                 , __outputFiles   :: [OutputPath]
                 , __captureStdout :: Bool
                 , __dependsOn     :: [Name] }
                 deriving (Show, Eq)

-- | Information about a submitted job.
data Status = Status { __spec  :: Spec
                     , __stage :: Stage
                     , __satis :: Satisfiability
                     , __uuid  :: UUID }
                     deriving (Show, Eq)

-- | 'Spec' not-so-smart constructor.
mkSpec :: Name          -- ^ 'name'
       -> Command       -- ^ 'command'
       -> Resources     -- ^ 'resources'
       -> [OutputPath]  -- ^ 'outputFiles'
       -> Bool          -- ^ 'captureStdout'
       -> [Name]        -- ^ 'dependsOn'
       -> Spec
mkSpec = Spec

-- | 'Status' not-so-smart constructor.
mkStatus :: Spec            -- ^ 'spec'
         -> Stage           -- ^ 'stage'
         -> Satisfiability  -- ^ 'satis'
         -> UUID            -- ^ 'uuid'
         -> Status
mkStatus = Status

-- | Names of 'Stage' constructors. Could probably be replaced by something
-- from "GHC.Generics".
majorStages :: [String]
majorStages = ["Queued", "Waiting", "Running", "Killing", "Finished", "Failed", "RunError"]

-- | The 'Stage' constructor name. Could probably be replaced by something
-- from "GHC.Generics".
majorStage :: Stage -> String
majorStage (Queued _)   = "Queued"
majorStage (Waiting _)  = "Waiting"
majorStage Running      = "Running"
majorStage Killing      = "Killing"
majorStage Finished     = "Finished"
majorStage (Failed _)   = "Failed"
majorStage (RunError _) = "RunError"

-- | Whether the constructor is 'Queued'.
isQueueStage :: Stage -> Bool
isQueueStage (Queued _) = True
isQueueStage _          = False

-- | Whether the stage is terminal. A terminal stage is steady and represents
-- the completion (successful or not) of a job.
isTerminationStage :: Stage -> Bool
isTerminationStage (Queued _)   = False
isTerminationStage (Waiting _)  = False
isTerminationStage Running      = False
isTerminationStage Killing      = False
isTerminationStage Finished     = True
isTerminationStage (Failed _)   = True
isTerminationStage (RunError _) = True

-- | Whether the constructor is queued or waiting on a dependency.
isPendingStage :: Stage -> Bool
isPendingStage (Queued _)  = True
isPendingStage (Waiting _) = True
isPendingStage _           = False

-- | > isActiveStage = not . 'isTerminationStage'
isActiveStage :: Stage -> Bool
isActiveStage = not . isTerminationStage

-- | Whether the job is known by the remote scheduler.
isOnScheduler :: Stage -> Bool
isOnScheduler (Queued LocalQueue) = False
isOnScheduler (Waiting _)         = False
isOnScheduler s                   = isActiveStage s

-- | Whether the Satisfiability is 'Unsatisfiable'.
isUnsatisfiable :: Satisfiability -> Bool
isUnsatisfiable (Unsatisfiable _) = True
isUnsatisfiable _                 = False

$(deriveJSON defaultOptions ''ExitCode)
$(deriveJSON defaultOptions{fieldLabelModifier = specJName} ''Spec)
$(deriveJSON defaultOptions{fieldLabelModifier = statusJName} ''Status)

$(makeLenses ''Spec)
$(makeLenses ''Status)

class (HasResources c) => HasSpec c where
    spec :: Lens' c Spec
    name :: Lens' c Name
    name = spec . _name
    -- | The bash command to execute.
    command :: Lens' c Command
    command = spec . _command
    -- | List of output files where the working directory is the root (@/@).
    outputFiles :: Lens' c [OutputPath]
    outputFiles = spec . _outputFiles
    -- | Whether to capture the standard output of the job as a file @stdout@.
    captureStdout :: Lens' c Bool
    captureStdout = spec . _captureStdout
    -- | The names of the jobs this job requires output from.
    dependsOn :: Lens' c [Name]
    dependsOn = spec . _dependsOn

class (HasSpec c) => HasStatus c where
    status :: Lens' c Status
    stage :: Lens' c Stage
    stage = status . _stage
    satis :: Lens' c Satisfiability
    satis = status . _satis
    -- | The UUID used to identify this job in the scheduler.
    uuid :: Lens' c UUID
    uuid = status . _uuid

instance HasResources Spec where resources = _resources
instance HasResources Status where resources = spec . resources
instance HasSpec Spec where spec = id
instance HasSpec Status where spec = _spec
instance HasStatus Status where status = id

instance ToJSON Stage where
    toJSON (Queued LocalQueue)     = object ["type" .= "Queued", "substage" .= "local"]
    toJSON (Queued SchedulerQueue) = object ["type" .= "Queued", "substage" .= "scheduler"]
    toJSON (Waiting n) = object ["type" .= "Waiting", "for" .= n]
    toJSON Running     = object ["type" .= "Running"]
    toJSON Killing     = object ["type" .= "Killing"]
    toJSON Finished    = object ["type" .= "Finished"]
    toJSON (RunError BadSchedulerTransition) = object ["type" .= "RunError", "reason" .= "BadSchedulerTransition"]
    toJSON (RunError SubexecutorFailure) = object ["type" .= "RunError", "reason" .= "SubexecutorFailure"]
    toJSON (RunError SchedulerLost) = object ["type" .= "RunError", "reason" .= "SchedulerLost"]
    toJSON (Failed UserKilled) = object ["type" .= "Failed", "reason" .= "UserKilled"]
    toJSON (Failed (NonZeroExitCode c)) = object ["type" .= "Failed", "reason" .= "NonZeroExitCode", "exitCode" .= c]
    toJSON (Failed MemoryExceeded) = object ["type" .= "Failed", "reason" .= "MemoryExceeded"]
    toJSON (Failed DiskExceeded) = object ["type" .= "Failed", "reason" .= "DiskExceeded"]
    toJSON (Failed TimeExceeded) = object ["type" .= "Failed", "reason" .= "TimeExceeded"]
    toJSON (Failed (DependencyFailed n)) = object ["type" .= "Failed", "reason" .= "DependencyFailed", "dependency" .= n]

instance FromJSON Stage where
    parseJSON (Aeson.Object v) = do
        typ <- v .: "type"
        case typ of
            "Queued"   -> v .: "substage" >>= \case
                "local"     -> pure $ Queued LocalQueue
                "scheduler" -> pure $ Queued SchedulerQueue
                _           -> fail "Invalid queue stage"
            "Waiting"  -> Waiting <$> v .: "for"
            "Running"  -> pure Running
            "Killing"  -> pure Killing
            "Finished" -> pure Finished
            "RunError" -> v .: "reason" >>= \case
                "BadSchedulerTransition" -> pure $ RunError BadSchedulerTransition
                "SubexecutorFailure"     -> pure $ RunError SubexecutorFailure
                "SchedulerLost"          -> pure $ RunError SchedulerLost
                _                        -> fail "Invalid run error reason"
            "Failed"   -> v .: "reason" >>= \case
                "UserKilled"       -> pure $ Failed UserKilled
                "MemoryExceeded"   -> pure $ Failed MemoryExceeded
                "DiskExceeded"     -> pure $ Failed DiskExceeded
                "TimeExceeded"     -> pure $ Failed TimeExceeded
                "NonZeroExitCode"  -> Failed . NonZeroExitCode <$> v .: "exitCode"
                "DependencyFailed" -> Failed . DependencyFailed <$> v .: "dependency"
                _                  -> fail "Invalid failure reason"
            _          -> fail "Invalid job stage type"
    parseJSON _ = fail "Invalid job stage value"

instance ToJSON Satisfiability where
    toJSON Satisfiable                                   =
        object [ "type" .= "Satisfiable" ]
    toJSON (Unsatisfiable InsufficientResources)         =
        object [ "type" .= "Unsatisfiable"
               , "reason" .= "InsufficientResources" ]
    toJSON (Unsatisfiable (DependenciesUnsatisfiable d)) =
        object [ "type" .= "Unsatisfiable"
               , "reason" .= "DependenciesUnsatisfiable"
               , "dependencies" .= d ]
    toJSON SatisfiabilityUnknown                         =
        object ["type" .= "SatisfiabilityUnknown"]

instance FromJSON Satisfiability where
    parseJSON (Aeson.Object v) = do
        typ <- v .: "type"
        case typ of
            "Satisfiable" -> pure Satisfiable
            "Unsatisfiable" -> v .: "reason" >>= \case
                "InsufficientResources"     -> pure $ Unsatisfiable InsufficientResources
                "DependenciesUnsatisfiable" -> Unsatisfiable <$> DependenciesUnsatisfiable <$> v .: "dependencies"
                _                           -> fail "Invalid job unsatisfiability reason"
            "SatisfiabilityUnknown" -> pure SatisfiabilityUnknown
            _ -> fail "Invalid job satisfiability type"
    parseJSON _ = fail "Invalid job satisfiability value"

instance FromJSON OutputPath where
    parseJSON (Aeson.String s) = toP . parseAbsFile $ T.unpack s
      where
        toP = either (fail . ("Output path: " ++) . displayException) (pure . OutputPath)
    parseJSON _                = fail "Output path must be string"

instance ToJSON OutputPath where
    toJSON = Aeson.String . T.pack . toFilePath . getOutputPath

instance FromJSON Name where
    parseJSON (Aeson.String s) = mkName s
    parseJSON _                = fail "Name must be string"

instance ToJSON Name where
    toJSON = Aeson.String . nameText

instance FromText Name where
    fromText = mkName

instance ToText Name where
    toText = nameText

-- | A 'Name' if the provided text is valid, or otherwise 'fail'.
mkName :: (MonadPlus m) => T.Text -> m Name
mkName s | s =~ [re|^[a-zA-Z0-9\._\-]+$|] = pure $ Name s
         | otherwise                      = fail "invalid name"

-- | All UUIDs are valid names.
uuidName :: UUID -> Name
uuidName = Name . T.pack . show

-- No Generic instance to preserve mkStatus invariants
instance NFData Status where
    rnf (Status a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

-- No Generic instance to preserve mkSpec invariants`
instance NFData Spec where
    rnf (Spec a b c d e f) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f

instance NFData Stage where  -- Using Generic instance
instance NFData QueueStage where  -- Using Generic instance
instance NFData RunErrorReason where  -- Using Generic instance
instance NFData FailureReason where  -- Using Generic instance
instance NFData Satisfiability where  -- Using Generic instance
instance NFData UnsatisfiableReason where  -- Using Generic instance
