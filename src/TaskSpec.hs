module TaskSpec (Name, Command, Resources (..), TaskSpec (..)) where

import qualified Data.Text.Lazy as L

import Units

type Name = L.Text
type Command = L.Text

data Resources = Resources { disk :: Value Double MB
                           , ram  :: Value Double MiB
                           , cpu  :: Value Double Core }

data TaskSpec = TaskSpec Name Command Resources
