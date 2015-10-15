{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Mock where

import Eclogues.API (AbsFile)
import Eclogues.AppConfig (AppConfig (AppConfig))
import qualified Eclogues.Job as Job
import Eclogues.Persist (withPersistDir)
import Eclogues.Scheduling.Command (schedulerJobUI)
import Eclogues.State (updateJobs)
import Eclogues.State.Monad (runState, appState)
import Eclogues.State.Types (AppState, jobs)
import Eclogues.Threads.Server (serve)
import Units (asVal, micro, second)

import Control.Concurrent (threadDelay)
import Control.Concurrent.AdvSTM (atomically)
import Control.Concurrent.AdvSTM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.AdvSTM.TChan (newTChanIO)
import Control.Concurrent.Async (waitAny, withAsync)
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Data.Default.Generics (def)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Word (Word16)
import Network.URI (URI (uriPath), escapeURIString, isUnescapedInURI, parseURI)
import Path (toFilePath, mkAbsDir)
import Path.IO (withSystemTempDirectory)

main :: IO ()
main = run (pure ()) "127.0.0.1" 8000

run :: IO () -> String -> Word16 -> IO ()
run bla host port' = withSystemTempDirectory "em" $ \d -> withPersistDir d $ \pctx -> lift $ do
    schedV <- newTChanIO
    let conf   = AppConfig jdir getURI schedV pctx jobURI outURI user
        jdir   = $(mkAbsDir "/mock/jobs")
        user   = "test"
        getURI = pure . Just . fromJust $ parseURI "http://localhost:8081/"
        jobURI = schedulerJobUI $ T.unpack user
        outURI = mkOutputURI . fromJust $ parseURI "http://localhost:8001/"
        port   = fromIntegral port'
    stateV <- newTVarIO def
    let web = serve bla host port conf stateV
        updater = forever $ do
            update stateV
            threadDelay . floor $ second (1 :: Double) `asVal` micro second

    putStrLn $ "Starting server on " ++ host ++ ':':show port
    withAsync web $ \webA -> withAsync updater $ \updaterA ->
        snd <$> waitAny [const undefined <$> webA, updaterA]

update :: TVar AppState -> IO ()
update stateV = atomically $ do
    state <- readTVar stateV
    let jerbs   = state ^. jobs
        (_, ts) = runState state . updateJobs jerbs $ changes jerbs
    writeTVar stateV $ ts ^. appState
  where
    changes _ = []

-- TODO: move somewhere else
-- | Append the job name and file path to the path of the job output server URI.
mkOutputURI :: URI -> Job.Name -> AbsFile -> URI
mkOutputURI pf name path = pf { uriPath = uriPath pf ++ name' ++ escapedPath }
  where
    escapedPath = escapeURIString isUnescapedInURI $ toFilePath path
    name' = T.unpack $ Job.nameText name
