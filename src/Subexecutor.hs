{-# LANGUAGE TemplateHaskell #-}

module Main where

import Eclogues.TaskSpec (Command, TaskSpec (..), Resources (..), RunResult (..))
import Eclogues.Util (readJSON, orError)
import Units

import Control.Applicative ((*>), pure)
import Control.Arrow ((&&&))
import Control.Conditional (condM, otherwiseM)
import Control.Monad (when)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.Text.Lazy as L
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, copyFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Path (copyDir)
import System.Process (callProcess, spawnProcess, waitForProcess)

data SubexecutorConfig = SubexecutorConfig { jobsDir :: FilePath }

$(deriveJSON defaultOptions ''SubexecutorConfig)

runCommand :: Value Int Second -> Command -> IO RunResult
runCommand timeout cmd = do
    proc <- spawnProcess "/usr/bin/timeout" [show (val timeout), "/bin/bash", "-c", L.unpack cmd]
    code <- waitForProcess proc
    pure $ case code of
        ExitFailure 124 -> Overtime
        c               -> Ended c

copyDirContents :: FilePath -> FilePath -> IO ()
copyDirContents from to = callProcess "/bin/cp" ["-r", from ++ "/.", to]

copyFileOrDir :: FilePath -> FilePath -> IO Bool
copyFileOrDir from to =
    condM [ (doesFileExist      from, copyFile from to' *> pure True)
          , (doesDirectoryExist from, copyDir  from to' *> pure True)
          , (otherwiseM             , pure False) ]
    where to' = to ++ from

runTask :: FilePath -> String -> IO ()
runTask path name = do
    spec <- orError =<< readJSON (specDir </> "spec.json")

    copyDirContents (specDir </> "workspace") "."
    createDirectoryIfMissing False depsDir
    mapM_ copyDep $ taskDependsOn spec

    runRes <- runCommand (time $ taskResources spec) (taskCommand spec)

    -- TODO: Trigger Failed state when output doesn't exist
    mapM_ (flip copyFileOrDir (specDir </> "output/")) $ taskOutputFiles spec
    writeFile (specDir </> "runresult") (show runRes)
    when (taskCaptureStdout spec) $ copyFile stdout $ specDir </> "output/stdout"
    where
        specDir = path </> name
        stdout = ".logs" </> name </> "0/stdout"
        depsDir = "./yb-dependencies/"
        depOutput n = path </> n </> "output"
        copyDep = uncurry copyDirContents . (depOutput &&& (depsDir ++)) . L.unpack

main :: IO ()
main = do
    conf <- orError =<< readJSON "/etc/eclogues/subexecutor.json"
    (name:_) <- getArgs
    runTask (jobsDir conf) name
