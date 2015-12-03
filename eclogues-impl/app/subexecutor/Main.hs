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

import Prelude hiding (writeFile)

import qualified Eclogues.Job as Job
import Eclogues.Paths (runResult, specFile)
import Eclogues.Util (
    AbsDir (..), RunResult (..), readJSON, orError, dirName, getOutputPath)

import Control.Arrow ((&&&))
import Control.Lens ((^.))
import Control.Monad (when)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveFromJSON, defaultOptions)
import Data.Maybe (fromMaybe)
import Data.Metrology.SI (Second (Second))
import Data.Metrology.Computing ((#>), Time)
import Data.Scientific.Suspicious (toBoundedInteger)
import qualified Data.Text as T
import Path ( Path, Abs, Dir, (</>), toFilePath
            , parseAbsFile, mkRelFile, mkRelDir )
import Path.IO (getCurrentDirectory, createDirectoryIfMissing, copyFile, writeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath.Glob (glob)
import System.Process (callProcess, spawnProcess, waitForProcess)

-- | JSON configuration type.
data SubexecutorConfig = SubexecutorConfig { jobsDir :: AbsDir }

$(deriveFromJSON defaultOptions ''SubexecutorConfig)

-- | Run a bash command with a time limit.
runCommand :: Time -> Job.Command -> IO RunResult
runCommand timeout cmd = do
    let time = fromMaybe maxBound . toBoundedInteger $ timeout #> Second :: Int
    proc <- spawnProcess "/usr/bin/timeout" [show time, "/bin/bash", "-c", T.unpack cmd]
    code <- waitForProcess proc
    pure $ case code of
        ExitFailure 124 -> Overtime
        c               -> Ended c

-- | Copy the contents of a directory into another.
copyDirContents :: Path Abs Dir -> Path Abs Dir -> IO ()
copyDirContents from to = callProcess "/bin/cp" ["-r", toFilePath from ++ ".", toFilePath to]

-- | Run a job from its spec.
runJob :: AbsDir    -- ^ Shared jobs directory
       -> Job.Name  -- ^ Job name
       -> IO ()
runJob (AbsDir shared) name = do
    cwd <- getCurrentDirectory
    let inputDir = cwd </> $(mkRelDir "virgil-dependencies/")
        copyDep = uncurry copyDirContents . (outputDir &&& (inputDir </>)) . dirName
        copyOutput f = copyFile (getOutputPath cwd f)
                                (getOutputPath (outputDir jobDirName) f)

    spec <- orError =<< readJSON (toFilePath $ specDir </> specFile) :: IO Job.Spec

    createDirectoryIfMissing False inputDir
    mapM_ copyDep $ spec ^. Job.dependsOn

    runRes <- runCommand (spec ^. Job.time) (spec ^. Job.command)

    -- TODO: Trigger Failed state when output doesn't exist
    mapM_ copyOutput $ spec ^. Job.outputFiles
    writeFile (specDir </> runResult) $ encode runRes
    when (spec ^. Job.captureStdout) $ glob ".logs/*/0/stdout" >>= \case
        [fp] | Just fn <- parseAbsFile fp
             -> copyFile fn $ outputDir jobDirName </> $(mkRelFile "stdout")
        _    -> error "Stdout log file missing"
  where
    specDir = shared </> jobDirName
    outputDir n = shared </> n </> $(mkRelDir "output/")
    jobDirName = dirName name

main :: IO ()
main = do
    conf <- orError =<< readJSON "/etc/xdg/eclogues/subexecutor.json"
    (nameStr:_) <- getArgs
    case Job.mkName $ T.pack nameStr of
        Nothing   -> error "Invalid job name"
        Just name -> runJob (jobsDir conf) name
