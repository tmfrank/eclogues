{-# LANGUAGE LambdaCase #-}

module Main where

import TaskSpec (TaskSpec (command))

import Prelude hiding (readFile)

import Control.Applicative (pure)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (readFile)
import qualified Data.Text.Lazy as L
import System.Environment (getArgs)
import System.Process (spawnCommand, waitForProcess)

readTaskSpec :: FilePath -> IO (Either String TaskSpec)
readTaskSpec = fmap eitherDecode . readFile

orError :: Either String a -> IO a
orError = \case
    Right a -> pure a
    Left  e -> error e

runTask :: FilePath -> String -> IO ()
runTask path name = do
    let dir = path ++ "/" ++ name
    spec <- orError =<< readTaskSpec (dir ++ "/spec.json")
    let cmd = command spec
    procH <- spawnCommand $ L.unpack cmd
    exitCode <- waitForProcess procH
    writeFile (dir ++ "/exitcode") (show exitCode)

main :: IO ()
main = do
    let path = "/vagrant/jobs"
    (name:_) <- getArgs
    runTask path name
