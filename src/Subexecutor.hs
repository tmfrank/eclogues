{-# LANGUAGE LambdaCase #-}

module Main where

import TaskSpec (Command, TaskSpec (..))

import Prelude hiding (readFile)

import Control.Applicative ((*>), pure)
import Control.Arrow ((&&&))
import Control.Conditional (condM, otherwiseM)
import Control.Monad ((<=<))
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (readFile)
import qualified Data.Text.Lazy as L
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, copyFile)
import System.Environment (getArgs)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.Path (copyDir)
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

copyFileOrDir :: FilePath -> FilePath -> IO Bool
copyFileOrDir from to =
    condM [ (doesFileExist      from, copyFile from to' *> pure True)
          , (doesDirectoryExist from, copyDir  from to' *> pure True)
          , (otherwiseM             , pure False) ]
    where to' = to ++ from

runTask :: FilePath -> String -> IO ()
runTask path name = do
    spec <- orError =<< readTaskSpec (specDir </> "spec.json")

    copyDirContents (specDir </> "workspace") "."
    createDirectoryIfMissing False depsDir
    mapM_ copyDep $ taskDependsOn spec

    exitCode <- runCommand $ taskCommand spec

    -- TODO: Trigger Failed state when output doesn't exist
    mapM_ (flip copyFileOrDir (specDir </> "output/")) $ taskOutputFiles spec
    writeFile (specDir </> "exitcode") (show exitCode)
    where
        specDir = path </> name
        depsDir = "./yb-dependencies/"
        depOutput n = path </> n </> "output"
        copyDep = uncurry copyDirContents . (depOutput &&& (depsDir ++)) . L.unpack

main :: IO ()
main = do
    let path = "/vagrant/jobs"
    (name:_) <- getArgs
    runTask path name
