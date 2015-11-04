{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

API executable entry point.
-}

module Main where

import Database.Zookeeper.Election (LeadershipError (..), whenLeader)
import Database.Zookeeper.ManagedEvents (ZKURI, ManagedZK, withZookeeper)
import Eclogues.API (AbsFile)
import Eclogues.AppConfig (AppConfig (AppConfig))
import qualified Eclogues.AppConfig as Config
import qualified Eclogues.Job as Job
import qualified Eclogues.Persist as Persist
import Eclogues.Scheduling.AuroraZookeeper (followAuroraMaster)
import Eclogues.Scheduling.Command (runScheduleCommand, schedulerJobUI)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types (AppState)
import Eclogues.Threads.Update (loadSchedulerState, monitorCluster)
import Eclogues.Threads.Server (serve)
import Eclogues.Util (AbsDir (getDir), readJSON, orError)
import Units

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.AdvSTM as STM
import Control.Concurrent.AdvSTM.TChan (newTChanIO, writeTChan, readTChan)
import Control.Concurrent.AdvSTM.TVar (newTVarIO)
import Control.Concurrent.Async (Async, waitAny, withAsync)
import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import Control.Exception (Exception, throwIO)
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, catchE)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveFromJSON, defaultOptions)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.Default.Generics (def)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word16)
import Database.Zookeeper (ZKError)
import Network.URI (URI (uriPath), escapeURIString, isUnescapedInURI)
import Path (toFilePath)
import Path.IO (createDirectoryIfMissing)
import Servant.Common.BaseUrl (BaseUrl (BaseUrl), Scheme (Http))
import System.IO (hPutStrLn, stderr)
import System.Random (mkStdGen, setStdGen)

-- | User-supplied API configuration.
data ApiConfig = ApiConfig { jobsDir :: AbsDir
                           , zookeeperHosts :: ZKURI
                           , bindAddress :: String
                           , bindPort :: Word16
                           , subexecutorUser :: T.Text
                           , outputUrlPrefix :: URI
                           , aegleAddress :: Maybe String
                           , aeglePort :: Maybe Int }

$(deriveFromJSON defaultOptions ''ApiConfig)

main :: IO ()
main = do
    seedStdGen
    apiConf <- orError =<< readJSON "/etc/xdg/eclogues/api.json"
    -- Used to prevent multiple web server threads from running at the same
    -- time. This can happen if the web thread doesn't finish dying between
    -- losing and regaining ZK election master status.
    webLock <- Lock.new
    res <- withZookeeper (zookeeperHosts apiConf) $ runExceptT . withZK apiConf webLock
    case res of
        Left (LZKError e)         -> error $ "Zookeeper coordination error: " ++ show e
        Left (ActionException ex) -> throwIO ex
        Left LeadershipLost       -> error "impossibru!"
        Right e                   -> error $ "Aurora ZK lookup error: " ++ show e

withZK :: ApiConfig -> Lock -> ManagedZK -> ExceptT LeadershipError IO ZKError
withZK apiConf webLock zk = whileLeader zk (advertisedData apiConf) $ do
    let jdir = getDir $ jobsDir apiConf
    schedV <- newTChanIO  -- Scheduler action channel
    (followAuroraFailure, getURI) <- followAuroraMaster zk "/aurora/scheduler"
    createDirectoryIfMissing False jdir
    Persist.withPersistDir jdir $ \pctx' -> do
        let conf   = AppConfig jdir getURI schedV pctx' jobURI outURI user monUrl
            user   = subexecutorUser apiConf
            jobURI = schedulerJobUI $ T.unpack user
            outURI = mkOutputURI $ outputUrlPrefix apiConf
            host   = bindAddress apiConf
            port   = fromIntegral $ bindPort apiConf
            monUrl = BaseUrl Http <$> aegleAddress apiConf <*> aeglePort apiConf
        lift $ withPersist host port conf webLock followAuroraFailure

withPersist :: String -> Int -> AppConfig -> Lock -> Async ZKError -> IO ZKError
withPersist host port conf webLock followAuroraFailure = do
    st <- loadFromDB conf
    stateV <- newTVarIO st
    clusterV <- newTVarIO Nothing
    let web = Lock.with webLock $ serve (pure ()) host port conf stateV clusterV
        updater = forever $ do
            loadSchedulerState conf stateV
            threadDelay . floor $ second (1 :: Double) `asVal` micro second
        monitor url = forever $ do
            monitorCluster url stateV clusterV
            threadDelay . floor $ second (30 :: Double) `asVal` micro second
        -- TODO: catch run error and reschedule
        enacter = forever . STM.atomically $ runSingleCommand conf

    -- Configure threads ignoring monitoring if the relevant configuration is not provided
    hPutStrLn stderr $ "Starting server on " ++ host ++ ':':show port
    withAsync web $ \webA -> withAsync updater $ \updaterA -> withAsync enacter $ \enacterA -> do
        let results = [followAuroraFailure, const undefined <$> webA, updaterA, enacterA]
        case Config.monitorUrl conf of
            Just url -> withAsync (monitor url) $ \mon -> snd <$> waitAny (results ++ [const undefined <$> mon])
            Nothing  -> hPutStrLn stderr warningMsg *> (snd <$> waitAny results)
                where warningMsg = "Unable to monitor cluster due to absent configuration"

-- | Contest Zookeeper election with the provided node data, and perform some
-- action while elected. If leadership is lost, wait until re-elected and
-- perform the action again. This function will never throw 'LeadershipLost'.
whileLeader :: ManagedZK -> BSS.ByteString -> IO a -> ExceptT LeadershipError IO a
whileLeader zk advertisedHost act =
    catchE (whenLeader zk "/eclogues" advertisedHost act) $ \case
        LeadershipLost -> whileLeader zk advertisedHost act
        e              -> throwE e

-- | Create encoded (host, port) to advertise via Zookeeper.
advertisedData :: ApiConfig -> BSS.ByteString
advertisedData (ApiConfig _ _ host port _ _ _ _) = BSL.toStrict $ encode (host, port)

-- | Append the job name and file path to the path of the job output server URI.
mkOutputURI :: URI -> Job.Name -> AbsFile -> URI
mkOutputURI pf name path = pf { uriPath = uriPath pf ++ name' ++ escapedPath }
  where
    escapedPath = escapeURIString isUnescapedInURI $ toFilePath path
    name' = T.unpack $ Job.nameText name

loadFromDB :: AppConfig -> IO AppState
loadFromDB conf = fmap ((^. ES.appState) . snd) . ES.runStateTS def $ do
    (js, cmds) <- lift . Persist.atomically (Config.pctx conf) $
        (,) <$> Persist.allJobs <*> Persist.allIntents
    ES.loadJobs js
    lift . STM.atomically $ mapM_ (writeTChan $ Config.schedChan conf) cmds

-- | Read a single scheduler command from 'Config.schedChan' and send it to
-- the scheduler, waiting if it is unavailable.
runSingleCommand :: AppConfig -> STM.AdvSTM ()
runSingleCommand conf = do
    cmd <- readTChan $ Config.schedChan conf
    schedConf <- Config.requireSchedConf conf
    STM.onCommit . throwExc $ do
        runScheduleCommand schedConf cmd
        lift . Persist.atomically (Config.pctx conf) $ Persist.deleteIntent cmd
  where
    throwExc :: (Exception e) => ExceptT e IO a -> IO a
    throwExc act = runExceptT act >>= \case
        Left e  -> throwIO e
        Right a -> pure a

-- | Seed the PRNG with the current time.
seedStdGen :: IO ()
seedStdGen = do
    curTime <- getPOSIXTime
    -- Unlikely to start twice within a millisecond
    let pico = floor (curTime * 1e3) :: Int
    setStdGen $ mkStdGen pico
