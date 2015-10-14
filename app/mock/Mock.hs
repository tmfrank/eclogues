{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Mock where

import Eclogues.API (AbsFile)
import Eclogues.AppConfig (AppConfig (AppConfig))
import qualified Eclogues.Job as Job
import Eclogues.Scheduling.Command (schedulerJobUI)
import Eclogues.Threads.Server (serve)
import Units (asVal, micro, second)

import Control.Concurrent (threadDelay)
import Control.Concurrent.AdvSTM.TVar (newTVarIO)
import Control.Concurrent.AdvSTM.TChan (newTChanIO)
import Control.Concurrent.Async (waitAny, withAsync)
import Control.Monad (forever)
import Data.Default.Generics (def)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Word (Word16)
import Network.URI (URI (uriPath), escapeURIString, isUnescapedInURI, parseURI)
import Path (toFilePath, mkAbsDir)

main :: IO ()
main = run (pure ()) "127.0.0.1" 8000

run :: IO () -> String -> Word16 -> IO ()
run bla host port' = do
    schedV <- newTChanIO
    let conf   = AppConfig jdir getURI schedV pctx jobURI outURI user
        jdir   = $(mkAbsDir "/mock/jobs")
        pctx   = error "tried to access persistence context"
        user   = "test"
        getURI = pure . Just . fromJust $ parseURI "http://localhost:8081/"
        jobURI = schedulerJobUI $ T.unpack user
        outURI = mkOutputURI . fromJust $ parseURI "http://localhost:8001/"
        port   = fromIntegral port'
    stateV <- newTVarIO def
    let web = serve bla host port conf stateV
        updater = forever $ do
            --loadSchedulerState conf stateV
            threadDelay . floor $ second (1 :: Double) `asVal` micro second

    putStrLn $ "Starting server on " ++ host ++ ':':show port
    withAsync web $ \webA -> withAsync updater $ \updaterA ->
        snd <$> waitAny [const undefined <$> webA, updaterA]

-- TODO: move somewhere else
-- | Append the job name and file path to the path of the job output server URI.
mkOutputURI :: URI -> Job.Name -> AbsFile -> URI
mkOutputURI pf name path = pf { uriPath = uriPath pf ++ name' ++ escapedPath }
  where
    escapedPath = escapeURIString isUnescapedInURI $ toFilePath path
    name' = T.unpack $ Job.nameText name
