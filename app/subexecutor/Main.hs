{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Entry point for the subexecutor, called by the executor on compute slaves.
-}

module Main where

import Eclogues.JobSpec (Command, RunResult (..), JobSpec)
import qualified Eclogues.JobSpec as Job
import Eclogues.Util (readJSON, orError)
import Units

import Control.Arrow ((&&&))
import Control.Conditional (condM, otherwiseM)
import Control.Lens ((^.))
import Control.Monad (when)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.Text.Lazy as L
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, copyFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import System.Process (callProcess, spawnProcess, waitForProcess)

-- | JSON configuration type.
data SubexecutorConfig = SubexecutorConfig { jobsDir :: FilePath }

$(deriveJSON defaultOptions ''SubexecutorConfig)

-- | Run a bash command with a time limit.
runCommand :: Value Int Second -> Command -> IO RunResult
runCommand timeout cmd = do
    proc <- spawnProcess "/usr/bin/timeout" [show (val timeout), "/bin/bash", "-c", L.unpack cmd]
    code <- waitForProcess proc
    pure $ case code of
        ExitFailure 124 -> Overtime
        c               -> Ended c

-- | Copy the contents of a directory into another.
copyDirContents :: FilePath -> FilePath -> IO ()
copyDirContents from to = callProcess "/bin/cp" ["-r", from ++ "/.", to]

-- | Copy a file or directory relative to the working directory to the same
-- relative path under another directory.
copyFileOrDir :: FilePath -- ^ Relative path of file or dir
              -> FilePath -- ^ Absolute path of target directory
              -> IO Bool  -- ^ True if copied, False if path doesn't exist
copyFileOrDir from to = go where
    go = condM [ (doesFileExist      from, copyFile from to' *> pure True)
               , (doesDirectoryExist from, copyDir           *> pure True)
               , (otherwiseM             , pure False) ]
    to' = to ++ from
    copyDir = callProcess "mkdir" ["-p", to'] *> copyDirContents from to'

-- | Run a job from its spec.
runJob :: FilePath -- ^ Shared jobs directory
       -> String   -- ^ Job name
       -> IO ()
runJob path name = do
    spec <- orError =<< readJSON (specDir </> "spec.json") :: IO JobSpec

    copyDirContents (specDir </> "workspace") "."
    createDirectoryIfMissing False depsDir
    mapM_ copyDep $ spec ^. Job.dependsOn

    runRes <- runCommand (spec ^. Job.time) (spec ^. Job.command)

    -- TODO: Trigger Failed state when output doesn't exist
    mapM_ (`copyFileOrDir` (specDir </> "output/")) $ spec ^. Job.outputFiles
    writeFile (specDir </> "runresult") (show runRes)
    when (spec ^. Job.captureStdout) $ glob ".logs/*/0/stdout" >>= \case
        [fn] -> copyFile fn $ specDir </> "output/stdout"
        _    -> error "Stdout log file missing"
    where
        specDir = path </> name
        depsDir = "./virgil-dependencies/"
        depOutput n = path </> n </> "output"
        copyDep = uncurry copyDirContents . (depOutput &&& (depsDir ++)) . L.unpack

main :: IO ()
main = do
    conf <- orError =<< readJSON "/etc/xdg/eclogues/subexecutor.json"
    (name:_) <- getArgs
    runJob (jobsDir conf) name
