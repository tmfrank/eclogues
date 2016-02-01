{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Eclogues.Mock (MockHelper, main, runningMock, waitForUpdate) where

import Eclogues.API (AbsFile)
import Eclogues.AppConfig (AppConfig (AppConfig))
import qualified Eclogues.Job as Job
import Eclogues.Persist (withPersistDir)
import Eclogues.State (updateJobs)
import Eclogues.State.Monad (runState, appState)
import Eclogues.State.Types (AppState, Jobs, jobs)
import Eclogues.Threads.Server (serve)

import Control.Concurrent (threadDelay)
import Control.Concurrent.AdvSTM (atomically)
import Control.Concurrent.AdvSTM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.AdvSTM.TChan (newTChanIO)
import Control.Concurrent.Async (waitAny, withAsync)
import qualified Control.Concurrent.Event as Event
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Data.Default.Generics (def)
import Data.HashMap.Lazy (keys)
import Data.Maybe (fromJust, fromMaybe)
import Data.Metrology ((%), (#))
import Data.Metrology.SI (Second (Second), micro)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Word (Word16)
import Network.URI (URI (uriPath), escapeURIString, isUnescapedInURI, parseURI)
import Path (toFilePath, mkAbsDir)
import Path.IO (withSystemTempDirectory)

main :: IO ()
main = run nope nope "127.0.0.1" 8000
  where
    nope = pure ()

run :: IO () -> IO () -> String -> Word16 -> IO ()
run bla ua host port' = withSystemTempDirectory "em" $ \d -> withPersistDir d $ \pctx -> lift $ do
    schedV <- newTChanIO
    let conf   = AppConfig jdir getURI schedV pctx fakeSchedulerJobUI outURI user (pure Nothing)
        jdir   = $(mkAbsDir "/mock/jobs")
        user   = "test"
        getURI = pure . Just . fromJust $ parseURI "http://localhost:8081/"
        outURI = mkOutputURI . fromJust $ parseURI "http://localhost:8001/"
        port   = fromIntegral port'
    stateV   <- newTVarIO def
    clusterV <- newTVarIO def
    let web = serve bla host port conf stateV clusterV
        updater = forever $ do
            update stateV
            ua
            threadDelay . floor $ ((1 % Second) # micro Second :: Double)

    putStrLn $ "Starting server on " ++ host ++ ':':show port
    withAsync web $ \webA -> withAsync updater $ \updaterA ->
        snd <$> waitAny [const undefined <$> webA, updaterA]

fakeSchedulerJobUI :: URI -> UUID -> URI
fakeSchedulerJobUI uri uuid = uri { uriPath = "/nonexistant-scheduler/" ++ show uuid }

update :: TVar AppState -> IO ()
update stateV = atomically $ do
    state <- readTVar stateV
    let jerbs   = state ^. jobs
        (_, ts) = runState state . updateJobs jerbs $ changes jerbs
    writeTVar stateV $ ts ^. appState

changes :: Jobs -> [(Job.Name, Job.Stage)]
changes jerbs = checkTheJerb <$> keys jerbs

checkTheJerb :: Job.Name -> (Job.Name, Job.Stage)
checkTheJerb jobName = (jobName, checkNamePrefix jobName)

checkNamePrefix :: Job.Name -> Job.Stage
checkNamePrefix name
    | check "will_crash"         = Job.Failed (Job.NonZeroExitCode 42)
    | check "will_succeed"       = Job.Finished
    | check "user_killed"        = Job.Failed Job.UserKilled
    | check "memory_exceeded"    = Job.Failed Job.MemoryExceeded
    | check "disk_exceeded"      = Job.Failed Job.DiskExceeded
    | check "time_exceeded"      = Job.Failed Job.TimeExceeded
    | check "failed_dependency"  = Job.Failed (Job.DependencyFailed $ forceName "failedDependency")
    | check "queued"             = Job.Queued Job.LocalQueue
    | otherwise                  = Job.Failed (Job.NonZeroExitCode 666)
  where
    check pre = (pre <> "_") `T.isPrefixOf` Job.nameText name

-- TODO: move somewhere else
-- | Append the job name and file path to the path of the job output server URI.
mkOutputURI :: URI -> Job.Name -> AbsFile -> URI
mkOutputURI pf name path = pf { uriPath = uriPath pf ++ name' ++ escapedPath }
  where
    escapedPath = escapeURIString isUnescapedInURI $ toFilePath path
    name' = T.unpack $ Job.nameText name

forceName :: T.Text -> Job.Name
forceName jName = fromMaybe (error $ "invalid test name " ++ show jName) $ Job.mkName jName

newtype MockHelper = MockHelper Event.Event

runningMock :: String -> Word16 -> (MockHelper -> IO a) -> IO a
runningMock apiHost apiPort a = do
    go <- Event.new
    ev <- Event.new
    let onStart  = Event.set go
        onUpdate = Event.signal ev
    withAsync (run onStart onUpdate apiHost apiPort) . const $ do
        Event.wait go
        a $ MockHelper ev

waitForUpdate :: MockHelper -> IO ()
waitForUpdate (MockHelper ev) = Event.wait ev
