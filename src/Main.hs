{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude hiding ((.))

import Database.Zookeeper.Election (LeadershipError (..), whenLeader)
import Database.Zookeeper.ManagedEvents (ManagedZK, withZookeeper)
import Eclogues.API (VAPI)
import Eclogues.ApiDocs (apiDocsHtml)
import Eclogues.AppConfig (AppConfig (AppConfig, schedChan), requireAurora)
import Eclogues.Instances ()
import Eclogues.Scheduling.AuroraZookeeper (followAuroraMaster)
import Eclogues.Scheduling.Command ( ScheduleConf (ScheduleConf), ScheduleCommand
                                   , runScheduleCommand, getSchedulerStatuses )
import Eclogues.State ( AppState, JobStatus (jobState), JobError (..), newAppState
                      , getJobs, activeJobs
                      , createJob, killJob, deleteJob, getJob, updateJobs )
import Eclogues.TaskSpec (JobState (..), FailureReason (..))
import Units

import Control.Applicative ((<$>), (<*), pure)
import Control.Category ((.))
import Control.Concurrent (threadDelay)
import Control.Concurrent.AdvSTM (atomically, onCommit)
import Control.Concurrent.AdvSTM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.AdvSTM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Concurrent.Async (waitAny, withAsync)
import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import Control.Exception (Exception, IOException, throwIO, try)
import Control.Monad (forever)
import Control.Monad.Morph (hoist, generalize)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, ExceptT, runExceptT, withExceptT, mapExceptT, throwE)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.HashMap.Lazy (keys)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word16)
import Database.Zookeeper (ZKError)
import Network.HTTP.Types (ok200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:<|>) ((:<|>)), Raw)
import Servant.Server (Server, ServerT, ServantErr (..), (:~>) (..)
                      , enter, fromExceptT
                      , serve, err404, err409)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

bool :: a -> a -> Bool -> a
bool a b p = if p then b else a

throwExc :: (Exception e) => ExceptT e IO a -> IO a
throwExc act = runExceptT act >>= \case
    Left e  -> throwIO e
    Right a -> pure a

type Scheduler = AppState -> Except JobError (AppState, [ScheduleCommand])

runScheduler :: AppConfig -> TVar AppState -> Scheduler -> ExceptT JobError IO ()
runScheduler conf stateV f = mapExceptT atomically $ do
    state <- lift $ readTVar stateV
    (state', cmds) <- hoist generalize $ f state
    lift $ mapM_ (writeTChan $ schedChan conf) cmds
    lift $ writeTVar stateV state'

mainServer :: AppConfig -> TVar AppState -> Server VAPI
mainServer conf stateV = enter (fromExceptT . Nat (withExceptT onError)) server where
    server :: ServerT VAPI (ExceptT JobError IO)
    server = getJobsH :<|> getJobH :<|> getJobStateH :<|> killJobH :<|> deleteJobH :<|> createJobH

    getJobsH = lift $ getJobs <$> atomically (readTVar stateV)
    getJobH jid = (hoist generalize . getJob jid) =<< lift (atomically $ readTVar stateV)
    getJobStateH = fmap jobState . getJobH
    createJobH = runScheduler' . createJob
    deleteJobH = runScheduler' . deleteJob

    killJobH jid (Failed UserKilled) = runScheduler' $ \st -> (st,) <$> killJob jid st
    killJobH jid _                   = throwE (InvalidStateTransition "Can only set state to Failed UserKilled") <* getJobH jid

    onError :: JobError -> ServantErr
    onError e = case e of
        NoSuchJob -> err404 { errBody = encode NoSuchJob }
        other     -> err409 { errBody = encode other }
    runScheduler' = runScheduler conf stateV

type VAPIWithDocs = VAPI :<|> Raw

docsServer :: Server VAPI -> Server VAPIWithDocs
docsServer = (:<|> serveDocs) where
    serveDocs _ respond = respond $ responseLBS ok200 [plain] apiDocsHtml
    plain = ("Content-Type", "text/html")

whileLeader :: ManagedZK -> ByteString -> IO a -> ExceptT LeadershipError IO a
whileLeader zk advertisedHost act =
    lift (runExceptT $ whenLeader zk "/eclogues" advertisedHost act) >>= \case
        Left LeadershipLost -> whileLeader zk advertisedHost act
        Left  e             -> throwE e
        Right a             -> pure a

withZK :: FilePath -> ByteString -> Lock -> ManagedZK -> ExceptT LeadershipError IO ZKError
withZK jobsDir advertisedHost webLock zk = whileLeader zk advertisedHost $ do
    (followAuroraFailure, getURI) <- followAuroraMaster zk "/aurora/scheduler"
    stateV <- newTVarIO newAppState
    schedV <- newTChanIO
    createDirectoryIfMissing False jobsDir

    let conf = AppConfig jobsDir getURI schedV

    let web = Lock.with webLock $ run 8000 . serve (Proxy :: (Proxy VAPIWithDocs)) . docsServer $ mainServer conf stateV
        updater = forever $ goUpdate >> threadDelay (floor $ second (1 :: Double) `asVal` micro second)
        goUpdate = do
            uri <- atomically $ requireAurora conf
            state <- atomically $ readTVar stateV
            let aJobs = activeJobs state
                sconf = ScheduleConf jobsDir uri
            newStatusesRes <- try . runExceptT . getSchedulerStatuses sconf $ keys aJobs
            case newStatusesRes of
                Right (Right newStatuses) -> atomically $ do
                    let (state', cmds) = updateJobs state aJobs newStatuses
                    mapM_ (writeTChan schedV) cmds
                    writeTVar stateV state'
                Left (ex :: IOException)  ->
                    hPutStrLn stderr $ "Error connecting to Aurora at " ++ show uri ++ "; retrying: " ++ show ex
                Right (Left resp)         -> throwIO resp
        enacter = forever . atomically $ do -- TODO: catch run error and reschedule
            cmd <- readTChan schedV
            auri <- requireAurora conf
            let sconf = ScheduleConf jobsDir auri
            onCommit . throwExc $ runScheduleCommand sconf cmd

    hPutStrLn stderr "Starting server on port 8000"
    withAsync web $ \webA -> withAsync updater $ \updaterA -> withAsync enacter $ \enacterA ->
        snd <$> waitAny [followAuroraFailure, const undefined <$> webA, updaterA, enacterA]

main :: IO ()
main = do
    (jobsDir:zkUri:myHost:_) <- getArgs
    let advertisedHost = toStrict $ encode ((myHost, 8000) :: (String, Word16))
    webLock <- Lock.new
    res <- withZookeeper zkUri $ runExceptT . withZK jobsDir advertisedHost webLock
    case res of
        Left (LZKError e)         -> error $ "Zookeeper coordination error: " ++ show e
        Left (ActionException ex) -> throwIO ex
        Left LeadershipLost       -> error "impossibru!"
        Right e                   -> error $ "Aurora ZK lookup error: " ++ show e
