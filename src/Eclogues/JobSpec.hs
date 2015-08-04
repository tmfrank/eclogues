{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Eclogues.JobSpec where

import Eclogues.JobSpec.Aeson

import Control.Lens.TH (makeClassy)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, deriveToJSON, defaultOptions, fieldLabelModifier)
import qualified Data.Text
import qualified Data.Text.Lazy as L
import Data.UUID (UUID)
import Data.UUID.Aeson ()
import System.Exit (ExitCode)

import Units

default (Data.Text.Text)

type Name = L.Text
type Command = L.Text

data Resources = Resources { _disk :: Value Double MB
                           , _ram  :: Value Double MiB
                           , _cpu  :: Value Double Core
                           , _time :: Value Int Second }
                           deriving (Show, Eq)

$(deriveToJSON defaultOptions{fieldLabelModifier = drop 1} ''Resources)
$(makeClassy ''Resources)

data JobSpec = JobSpec { _name          :: Name
                       , _command       :: Command
                       , _job_resources :: Resources
                       , _outputFiles   :: [FilePath]
                       , _captureStdout :: Bool
                       , _dependsOn     :: [Name] }
                         deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = specJName} ''JobSpec)
$(makeClassy ''JobSpec)
instance HasResources JobSpec where resources = job_resources

data RunResult = Ended ExitCode | Overtime deriving (Show, Read)

data FailureReason = UserKilled
                   | NonZeroExitCode Int
                   | MemoryExceeded --(Value Double Byte)
                   | DiskExceeded --(Value Double Byte)
                   | TimeExceeded
                   | DependencyFailed Name
                   deriving (Show, Eq)

data RunErrorReason = BadSchedulerTransition
                    | SubexecutorFailure
                    | SchedulerLost
                    deriving (Show, Eq)

data QueueStage = LocalQueue | SchedulerQueue deriving (Show, Eq)

data JobState = Queued QueueStage
              | Waiting Integer
              | Running
              | Killing
              | Finished
              | Failed FailureReason
              | RunError RunErrorReason
                deriving (Show, Eq)

data JobStatus = JobStatus { _job_spec :: JobSpec
                           , _jobState :: JobState
                           , _uuid     :: UUID }
                           deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = statusJName} ''JobStatus)
$(makeClassy ''JobStatus)
instance HasJobSpec JobStatus where jobSpec = job_spec

majorJobStates :: [String]
majorJobStates = ["Queued", "Waiting", "Running", "Killing", "Finished", "Failed", "RunError"]

majorState :: JobState -> String
majorState (Queued _)   = "Queued"
majorState (Waiting _)  = "Waiting"
majorState Running      = "Running"
majorState Killing      = "Killing"
majorState Finished     = "Finished"
majorState (Failed _)   = "Failed"
majorState (RunError _) = "RunError"

isQueueState :: JobState -> Bool
isQueueState (Queued _) = True
isQueueState _          = False

isTerminationState :: JobState -> Bool
isTerminationState (Queued _)   = False
isTerminationState (Waiting _)  = False
isTerminationState Running      = False
isTerminationState Killing      = False
isTerminationState Finished     = True
isTerminationState (Failed _)   = True
isTerminationState (RunError _) = True

isActiveState :: JobState -> Bool
isActiveState = not . isTerminationState

isOnScheduler :: JobState -> Bool
isOnScheduler (Queued LocalQueue) = False
isOnScheduler (Waiting _)         = False
isOnScheduler s                   = isActiveState s

isExpectedTransition :: JobState -> JobState -> Bool
isExpectedTransition (Queued LocalQueue) (Queued SchedulerQueue) = True
isExpectedTransition (Queued _)   Running       = True
isExpectedTransition (Waiting 0)  Running       = True
isExpectedTransition Running      (Queued SchedulerQueue) = True
isExpectedTransition Killing      (Failed UserKilled) = True
isExpectedTransition o n | isQueueState o || o == Running = case n of
                                  Finished     -> True
                                  (Failed _)   -> True
                                  (RunError _) -> True
                                  _            -> False
isExpectedTransition _            _             = False

instance ToJSON JobState where
    toJSON (Queued LocalQueue)     = object ["type" .= "Queued", "stage" .= "local"]
    toJSON (Queued SchedulerQueue) = object ["type" .= "Queued", "stage" .= "scheduler"]
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

instance FromJSON JobState where
    parseJSON (Aeson.Object v) = do
        typ <- v .: "type"
        case typ of
            "Queued"   -> v .: "stage" >>= \case
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
            _          -> fail "Invalid job state type"
    parseJSON _ = fail "Invalid job state value"

instance FromJSON Resources where
    parseJSON = validate <=< getJSON where
        getJSON (Aeson.Object v) = Resources <$> v .: "disk" <*> v .: "ram" <*> v .: "cpu" <*> v .: "time"
        getJSON _                = fail "Invalid resources value"
        validate res@(Resources dsk rm cu te) =
            if any ((== -1) . signum) [val dsk, val rm, val cu, fromIntegral (val te)]
                then fail "Negative resource value"
                else pure res