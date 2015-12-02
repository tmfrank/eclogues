{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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
    -- * Job satisfiability
      Satisfiability (..), UnsatisfiableReason (..), isUnsatisfiable
    -- * Job status
    , Status (Status), HasStatus (..)
    -- ** Job spec
    , Spec (Spec), HasSpec (..)
    , Name, nameText, mkName, uuidName
    , Command
    , OutputPath (..)
    -- ** Resources
    , module Eclogues.Job.Resources
    -- ** Job lifecycle stage
    , Stage (..), RunResult (..), FailureReason (..), RunErrorReason (..), QueueStage (..)
    , majorStage, majorStages
    -- * Predicates
    , isActiveStage, isTerminationStage, isOnScheduler, isExpectedTransition, isPendingStage
    ) where

import Eclogues.Job.Aeson
import Eclogues.Job.Resources

import Control.Exception (displayException)
import Control.Lens.TH (makeClassy)
import Control.Monad (MonadPlus)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Hashable (Hashable)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.UUID.Aeson ()
import Path (Path, Abs, File, parseAbsFile, toFilePath)
import Servant.Common.Text (FromText (..), ToText (..))
import System.Exit (ExitCode)
import Text.Regex.PCRE.Heavy ((=~), re)

default (T.Text)

newtype Name = Name { _nameText :: T.Text } deriving (Eq, Hashable)

-- Don't want to export a record field
nameText :: Name -> T.Text
nameText = _nameText

instance Show Name where
    show = show . nameText

instance Ord Name where
    compare = comparing _nameText

type Command = T.Text

newtype OutputPath = OutputPath { getOutputPath :: Path Abs File }
                     deriving (Show, Eq)

data Spec = Spec { _name          :: Name
                 , _command       :: Command
                 , __resources    :: Resources
                 , _outputFiles   :: [OutputPath]
                 , _captureStdout :: Bool
                 , _dependsOn     :: [Name] }
                 deriving (Show, Eq)

-- | The result of a job, as communicated by the subexecutor. Other failure
-- modes are communicated by the scheduler.
data RunResult = Ended ExitCode | Overtime deriving (Show)

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

data Stage = Queued QueueStage
           | Waiting Integer
           | Running
           | Killing
           | Finished
           | Failed FailureReason
           | RunError RunErrorReason
           deriving (Show, Eq)

data Satisfiability = Satisfiable
                    | Unsatisfiable UnsatisfiableReason
                    | SatisfiabilityUnknown
                      deriving (Show, Eq)

data UnsatisfiableReason = InsufficientResources
                         | DependenciesUnsatisfiable [Name]
                           deriving (Show, Eq)

data Status = Status { __spec :: Spec
                     , _stage :: Stage
                     , _satis :: Satisfiability
                     , _uuid  :: UUID }
                     deriving (Show, Eq)

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

-- | Whether the first 'Stage' may transition to the second in the lifecycle.
-- Unexpected transitions are treated as scheduler errors
-- ('BadSchedulerTransition').
isExpectedTransition :: Stage -> Stage -> Bool
isExpectedTransition (Queued LocalQueue) (Queued SchedulerQueue) = True
isExpectedTransition (Queued _)   Running       = True
isExpectedTransition (Waiting 0)  Running       = True
isExpectedTransition Running      (Queued SchedulerQueue) = True
isExpectedTransition Killing      (Failed UserKilled) = True
isExpectedTransition o n | isQueueStage o || o == Running = case n of
                                  Finished     -> True
                                  (Failed _)   -> True
                                  (RunError _) -> True
                                  _            -> False
isExpectedTransition _            _             = False


-- | Whether the Satisfiability is Unsatisfiable.
isUnsatisfiable :: Satisfiability -> Bool
isUnsatisfiable (Unsatisfiable _) = True
isUnsatisfiable _                 = False

$(deriveJSON defaultOptions ''ExitCode)
$(deriveJSON defaultOptions ''RunResult)
$(deriveJSON defaultOptions{fieldLabelModifier = specJName} ''Spec)
$(deriveJSON defaultOptions{fieldLabelModifier = statusJName} ''Status)

$(makeClassy ''Spec)
$(makeClassy ''Status)
instance HasResources Spec where resources = _resources
instance HasSpec Status where spec = _spec

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
    parseJSON (Aeson.String s) = toP $ parseAbsFile $ T.unpack s
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

mkName :: (MonadPlus m) => T.Text -> m Name
mkName s | s =~ [re|^[a-zA-Z0-9\._\-]+$|] = pure $ Name s
         | otherwise                      = fail "invalid name"

uuidName :: UUID -> Name
uuidName = Name . T.pack . show
