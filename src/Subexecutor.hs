{-# LANGUAGE TemplateHaskell #-}

module Main where

import Eclogues.TaskSpec ( Command, RunResult (..)
                         , TaskSpec, taskDependsOn, taskCommand, taskOutputFiles, taskCaptureStdout, time )
import Eclogues.Util (readJSON, orError)
import Units

import Control.Applicative ((*>), pure)
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
copyFileOrDir from to = go where
    go = condM [ (doesFileExist      from, copyFile from to' *> pure True)
               , (doesDirectoryExist from, copyDir           *> pure True)
               , (otherwiseM             , pure False) ]
    to' = to ++ from
    copyDir = callProcess "mkdir" ["-p", to'] *> copyDirContents from to'

runTask :: FilePath -> String -> IO ()
runTask path name = do
    spec <- orError =<< readJSON (specDir </> "spec.json") :: IO TaskSpec

    copyDirContents (specDir </> "workspace") "."
    createDirectoryIfMissing False depsDir
    mapM_ copyDep $ spec ^. taskDependsOn

    runRes <- runCommand (spec ^. time) (spec ^. taskCommand)

    -- TODO: Trigger Failed state when output doesn't exist
    mapM_ (flip copyFileOrDir (specDir </> "output/")) $ spec ^. taskOutputFiles
    writeFile (specDir </> "runresult") (show runRes)
    when (spec ^. taskCaptureStdout) $ glob ".logs/*/0/stdout" >>= \case
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
    runTask (jobsDir conf) name
