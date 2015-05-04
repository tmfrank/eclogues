{-# LANGUAGE TemplateHaskell #-}

module TaskSpec where

import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)
import qualified Data.Text.Lazy as L

import Units

type Name = L.Text
type Command = L.Text

data Resources = Resources { disk :: Value Double MB
                           , ram  :: Value Double MiB
                           , cpu  :: Value Double Core }
                           deriving (Show, Eq)

data TaskSpec = TaskSpec { taskName      :: Name
                         , taskCommand   :: Command
                         , taskResources :: Resources }
                         deriving (Show, Eq)

data FailureReason = UserKilled
                   | NonZeroExitCode Int
                   | MemoryExceeded --(Value Double Byte)
                   | DiskExceeded --(Value Double Byte)
                   deriving (Show, Eq)

data JobState = Waiting | Running | Finished | Failed FailureReason | RunError
                deriving (Show, Eq)

isTerminationState :: JobState -> Bool
isTerminationState Waiting    = False
isTerminationState Running    = False
isTerminationState Finished   = True
isTerminationState (Failed _) = True
isTerminationState RunError   = True

isActiveState :: JobState -> Bool
isActiveState = not . isTerminationState

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 4} ''TaskSpec)
$(deriveJSON defaultOptions ''Resources)
$(deriveJSON defaultOptions ''FailureReason)
$(deriveJSON defaultOptions ''JobState)
