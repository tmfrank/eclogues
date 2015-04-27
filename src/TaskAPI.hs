{-# LANGUAGE OverloadedStrings #-}

module TaskAPI where

import Api_Types (Response)

import qualified AuroraAPI as A
import TaskSpec (TaskSpec (name, command))

import Prelude hiding (writeFile)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT)
import Data.Aeson (encode)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as L
import Data.ByteString.Lazy (writeFile)
import System.Directory (createDirectoryIfMissing)

data CreateJobError = UnknownResponse Response
                      deriving (Show)

createJob :: FilePath -> A.Client -> TaskSpec -> ExceptT CreateJobError IO ()
createJob root client spec = do
    let dir = root ++ "/" ++ (L.unpack $ name spec)
    lift $ createDirectoryIfMissing False dir
    lift $ createDirectoryIfMissing False $ dir ++ "/workspace"
    lift $ writeFile (dir ++ "/spec.json") (encode spec)
    let subspec = spec { command = "aurora-subexecutor " <> name spec }
    withExceptT UnknownResponse . ExceptT $ A.createJob client subspec
