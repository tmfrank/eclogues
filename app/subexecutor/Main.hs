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
import Control.Exception (displayException)
import Control.Lens ((^.))
import Control.Monad (when)
import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveFromJSON, defaultOptions)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Path ( Path, Abs, Dir, File, (</>), toFilePath
            , parseAbsDir, parseAbsFile, mkRelFile, mkRelDir )
import qualified System.Directory as D
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath.Glob (glob)
import System.Process (callProcess, spawnProcess, waitForProcess)

newtype AbsDir = AbsDir { getDir :: Path Abs Dir }

-- | JSON configuration type.
data SubexecutorConfig = SubexecutorConfig { jobsDir :: AbsDir }

$(deriveFromJSON defaultOptions ''SubexecutorConfig)

-- | Run a bash command with a time limit.
runCommand :: Value Int Second -> Command -> IO RunResult
runCommand timeout cmd = do
    proc <- spawnProcess "/usr/bin/timeout" [show (val timeout), "/bin/bash", "-c", L.unpack cmd]
    code <- waitForProcess proc
    pure $ case code of
        ExitFailure 124 -> Overtime
        c               -> Ended c

-- | Copy the contents of a directory into another.
copyDirContents :: Path Abs Dir -> Path Abs Dir -> IO ()
copyDirContents from to = callProcess "/bin/cp" ["-r", toFilePath from ++ ".", toFilePath to]

copyFile :: Path Abs File -> Path Abs a -> IO ()
copyFile a = D.copyFile (toFilePath a) . toFilePath

-- | Run a job from its spec.
runJob :: AbsDir    -- ^ Shared jobs directory
       -> Job.Name  -- ^ Job name
       -> IO ()
runJob (AbsDir shared) name = do
    cwd <- parseAbsDir =<< D.getCurrentDirectory
    let inputDir = cwd </> $(mkRelDir "virgil-dependencies/")
        copyDep = uncurry copyDirContents . (outputDir &&& (inputDir </>)) . Job.dirName
        copyOutput f = copyFile (Job.getOutputPath cwd f)
                                (Job.getOutputPath (outputDir jobDirName) f)

    spec <- orError =<< readJSON (toFilePath specFile) :: IO JobSpec

    D.createDirectoryIfMissing False $ toFilePath inputDir
    mapM_ copyDep $ spec ^. Job.dependsOn

    runRes <- runCommand (spec ^. Job.time) (spec ^. Job.command)

    -- TODO: Trigger Failed state when output doesn't exist
    mapM_ copyOutput $ spec ^. Job.outputFiles
    writeFile (toFilePath $ specDir </> $(mkRelFile "runresult")) (show runRes)
    when (spec ^. Job.captureStdout) $ glob ".logs/*/0/stdout" >>= \case
        [fp] | Just fn <- parseAbsFile fp
             -> copyFile fn $ outputDir jobDirName </> $(mkRelFile "stdout")
        _    -> error "Stdout log file missing"
  where
    specDir = shared </> jobDirName
    specFile = specDir </> $(mkRelFile "spec.json")
    outputDir n = shared </> n </> $(mkRelDir "output/")
    jobDirName = Job.dirName name

main :: IO ()
main = do
    conf <- orError =<< readJSON "/etc/xdg/eclogues/subexecutor.json"
    (name:_) <- getArgs
    runJob (jobsDir conf) $ L.pack name

instance FromJSON AbsDir where
    parseJSON (Aeson.String s) = toP $ parseAbsDir $ T.unpack s
      where
        toP = either (fail . ("Absolute dir: " ++) . displayException) (pure . AbsDir)
    parseJSON _                = fail "Absolute dir must be string"
