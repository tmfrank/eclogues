{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TaskSpec where

import Control.Applicative ((<$>), pure)
import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)
import qualified Data.Text
import qualified Data.Text.Lazy as L

import Units

default (Data.Text.Text)

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

instance ToJSON JobState where
    toJSON Waiting  = object ["type" .= "Waiting"]
    toJSON Running  = object ["type" .= "Running"]
    toJSON Finished = object ["type" .= "Finished"]
    toJSON RunError = object ["type" .= "RunError"]
    toJSON (Failed UserKilled) = object ["type" .= "Failed", "reason" .= "UserKilled"]
    toJSON (Failed (NonZeroExitCode c)) = object ["type" .= "Failed", "reason" .= "NonZeroExitCode", "exitCode" .= c]
    toJSON (Failed MemoryExceeded) = object ["type" .= "Failed", "reason" .= "MemoryExceeded"]
    toJSON (Failed DiskExceeded) = object ["type" .= "Failed", "reason" .= "DiskExceeded"]

instance FromJSON JobState where
    parseJSON (Aeson.Object v) = do
        typ <- v .: "type"
        case typ of
            "Waiting"  -> pure Waiting
            "Running"  -> pure Running
            "Finished" -> pure Finished
            "RunError" -> pure RunError
            "Failed"   -> v .: "reason" >>= \case
                "UserKilled"      -> pure $ Failed UserKilled
                "MemoryExceeded"  -> pure $ Failed MemoryExceeded
                "DiskExceeded"    -> pure $ Failed DiskExceeded
                "NonZeroExitCode" -> Failed <$> NonZeroExitCode <$> v .: "exitCode"
                _                 -> mzero
            _          -> mzero
    parseJSON _ = mzero

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 4} ''TaskSpec)
$(deriveJSON defaultOptions ''Resources)
