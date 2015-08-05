{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Database.Zookeeper.Election (LeadershipError (..), whenLeader)
import Database.Zookeeper.ManagedEvents (ZKURI, ManagedZK, withZookeeper)
import Eclogues.API (AbsFile)
import Eclogues.AppConfig (AppConfig (AppConfig))
import qualified Eclogues.AppConfig as Config
import qualified Eclogues.JobSpec as Job
import qualified Eclogues.Persist as Persist
import Eclogues.Scheduling.AuroraZookeeper (followAuroraMaster)
import Eclogues.Scheduling.Command (runScheduleCommand, schedulerJobUI)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types (AppState)
import Eclogues.Threads.Update (loadSchedulerState)
import Eclogues.Threads.Server (serve)
import Eclogues.Util (readJSON, orError)
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
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.Default.Generics (def)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word16)
import Database.Zookeeper (ZKError)
import Network.URI (URI (uriPath), escapeURIString, isUnescapedInURI)
import Path (toFilePath)
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import System.Random (mkStdGen, setStdGen)

data ApiConfig = ApiConfig { jobsDir :: FilePath
                           , zookeeperHosts :: ZKURI
                           , bindAddress :: String
                           , bindPort :: Word16
                           , subexecutorUser :: TS.Text
                           , outputUrlPrefix :: URI }

$(deriveJSON defaultOptions ''ApiConfig)

main :: IO ()
main = do
    seedStdGen
    apiConf <- orError =<< readJSON "/etc/xdg/eclogues/api.json"
    webLock <- Lock.new
    res <- withZookeeper (zookeeperHosts apiConf) $ runExceptT . withZK apiConf webLock
    case res of
        Left (LZKError e)         -> error $ "Zookeeper coordination error: " ++ show e
        Left (ActionException ex) -> throwIO ex
        Left LeadershipLost       -> error "impossibru!"
        Right e                   -> error $ "Aurora ZK lookup error: " ++ show e

withZK :: ApiConfig -> Lock -> ManagedZK -> ExceptT LeadershipError IO ZKError
withZK apiConf webLock zk = whileLeader zk (advertisedData apiConf) $ do
    let jdir = jobsDir apiConf
    schedV <- newTChanIO
    (followAuroraFailure, getURI) <- followAuroraMaster zk "/aurora/scheduler"
    createDirectoryIfMissing False jdir
    Persist.withPersistDir jdir $ \pctx' -> do
        let conf   = AppConfig jdir getURI schedV pctx' jobURI outURI user
            user   = subexecutorUser apiConf
            jobURI = schedulerJobUI $ TS.unpack user
            outURI = mkOutputURI $ outputUrlPrefix apiConf
        lift $ withPersist conf webLock followAuroraFailure

withPersist :: AppConfig -> Lock -> Async ZKError -> IO ZKError
withPersist conf webLock followAuroraFailure = do
    st <- loadFromDB conf
    stateV <- newTVarIO st
    let web = Lock.with webLock $ serve 8000 conf stateV
        updater = forever $ do
            loadSchedulerState conf stateV
            threadDelay . floor $ second (1 :: Double) `asVal` micro second
        -- TODO: catch run error and reschedule
        enacter = forever . STM.atomically $ runSingleCommand conf

    hPutStrLn stderr "Starting server on port 8000"
    withAsync web $ \webA -> withAsync updater $ \updaterA -> withAsync enacter $ \enacterA ->
        snd <$> waitAny [followAuroraFailure, const undefined <$> webA, updaterA, enacterA]

throwExc :: (Exception e) => ExceptT e IO a -> IO a
throwExc act = runExceptT act >>= \case
    Left e  -> throwIO e
    Right a -> pure a

whileLeader :: ManagedZK -> BSS.ByteString -> IO a -> ExceptT LeadershipError IO a
whileLeader zk advertisedHost act =
    lift (runExceptT $ whenLeader zk "/eclogues" advertisedHost act) >>= \case
        Left LeadershipLost -> whileLeader zk advertisedHost act
        Left  e             -> throwE e
        Right a             -> pure a

advertisedData :: ApiConfig -> BSS.ByteString
advertisedData (ApiConfig _ _ host port _ _) = BSL.toStrict $ encode (host, port)

mkOutputURI :: URI -> Job.Name -> AbsFile -> URI
mkOutputURI pf name path = pf { uriPath = (uriPath pf) ++ TL.unpack name ++ escapedPath }
  where
    escapedPath = escapeURIString isUnescapedInURI $ toFilePath path

loadFromDB :: AppConfig -> IO AppState
loadFromDB conf = fmap ((^. ES.appState) . snd) . ES.runStateTS def $ do
    (js, cmds) <- lift . Persist.atomically (Config.pctx conf) $
        (,) <$> Persist.allJobs <*> Persist.allIntents
    ES.loadJobs js
    lift . STM.atomically $ mapM_ (writeTChan $ Config.schedChan conf) cmds

runSingleCommand :: AppConfig -> STM.AdvSTM ()
runSingleCommand conf = do
    cmd <- readTChan $ Config.schedChan conf
    schedConf <- Config.requireSchedConf conf
    STM.onCommit . throwExc $ do
        runScheduleCommand schedConf cmd
        lift . Persist.atomically (Config.pctx conf) $ Persist.deleteIntent cmd

seedStdGen :: IO ()
seedStdGen = do
    curTime <- getPOSIXTime
    -- Unlikely to start twice within a millisecond
    let pico = floor (curTime * 1e3) :: Int
    setStdGen $ mkStdGen pico
