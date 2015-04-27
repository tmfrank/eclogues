{-# LANGUAGE LambdaCase #-}

module Main where

import TaskSpec (Command, TaskSpec (command))

import Prelude hiding (readFile)

import Control.Applicative (pure)
import Control.Monad ((<=<))
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (readFile)
import qualified Data.Text.Lazy as L
import System.Environment (getArgs)
import System.Exit (ExitCode)
import System.Process (callProcess, spawnCommand, waitForProcess)

readTaskSpec :: FilePath -> IO (Either String TaskSpec)
readTaskSpec = fmap eitherDecode . readFile

orError :: Either String a -> IO a
orError = \case
    Right a -> pure a
    Left  e -> error e

runCommand :: Command -> IO ExitCode
runCommand = waitForProcess <=< spawnCommand . L.unpack

copyDirContents :: FilePath -> FilePath -> IO ()
copyDirContents from to = callProcess "/bin/cp" ["-r", from ++ "/.", to]

runTask :: FilePath -> String -> IO ()
runTask path name = do
    let dir = path ++ "/" ++ name
    spec <- orError =<< readTaskSpec (dir ++ "/spec.json")

    let wsdir = dir ++ "/workspace"
    copyDirContents wsdir "."
    exitCode <- runCommand $ command spec
    copyDirContents "." wsdir

    writeFile (dir ++ "/exitcode") (show exitCode)

main :: IO ()
main = do
    let path = "/vagrant/jobs"
    (name:_) <- getArgs
    runTask path name
