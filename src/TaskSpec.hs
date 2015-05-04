{-# LANGUAGE TemplateHaskell #-}

module TaskSpec where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.Text.Lazy as L

import Units

type Name = L.Text
type Command = L.Text

data Resources = Resources { disk :: Value Double MB
                           , ram  :: Value Double MiB
                           , cpu  :: Value Double Core }
                           deriving (Show, Eq)

data TaskSpec = TaskSpec { name      :: Name
                         , command   :: Command
                         , resources :: Resources }
                         deriving (Show, Eq)

data FailureReason = UserKilled
                   | NonZeroExitCode Int
                   | MemoryExceeded --(Value Double Byte)
                   | DiskExceeded --(Value Double Byte)
                   deriving (Show, Eq)

data JobState = Waiting | Running | Finished | Failed FailureReason | RunError
                deriving (Show, Eq)

$(deriveJSON defaultOptions ''TaskSpec)
$(deriveJSON defaultOptions ''Resources)
$(deriveJSON defaultOptions ''FailureReason)
$(deriveJSON defaultOptions ''JobState)
