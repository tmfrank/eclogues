{-# LANGUAGE LambdaCase #-}

module Main where

import TaskSpec (Command, TaskSpec (..))

import Prelude hiding (readFile)

import Control.Applicative (pure)
import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (readFile)
import qualified Data.Text.Lazy as L
import System.Directory (createDirectoryIfMissing)
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
    spec <- orError =<< readTaskSpec (specDir ++ "/spec.json")

    copyDirContents (ws name) "."
    createDirectoryIfMissing False depDir
    mapM_ copyDep $ taskDependsOn spec

    exitCode <- runCommand $ taskCommand spec

    copyDirContents "." (ws name)
    writeFile (specDir ++ "/exitcode") (show exitCode)
    where
        specDir = path ++ "/" ++ name
        depDir = "./yb-dependencies/"
        ws n = path ++ "/" ++ n ++ "/workspace"
        copyDep = uncurry copyDirContents . (ws &&& (depDir ++)) . L.unpack

main :: IO ()
main = do
    let path = "/vagrant/jobs"
    (name:_) <- getArgs
    runTask path name
