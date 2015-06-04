{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude hiding ((.))

import Database.Zookeeper.Election (LeadershipError (..), whenLeader)
import Database.Zookeeper.ManagedEvents (ZKURI, ManagedZK, withZookeeper)
import Eclogues.API (VAPI, JobError (..))
import Eclogues.ApiDocs (apiDocsHtml)
import Eclogues.AppConfig (AppConfig (AppConfig, schedChan, pctx), requireAurora)
import Eclogues.Instances ()
import Eclogues.Persist (PersistContext)
import qualified Eclogues.Persist as Persist
import Eclogues.Scheduling.AuroraZookeeper (followAuroraMaster)
import Eclogues.Scheduling.Command ( ScheduleConf (ScheduleConf)
                                   , runScheduleCommand, getSchedulerStatuses )
import Eclogues.State (getJobs, activeJobs, createJob, killJob, deleteJob, getJob, updateJobs)
import Eclogues.State.Monad (EState, runEState, scheduleCommands, appState, persist)
import Eclogues.State.Types (AppState, newAppState)
import Eclogues.TaskSpec (JobState (..), FailureReason (..), jobState)
import Eclogues.Util (readJSON, orError)
import Units

import Control.Applicative ((<$>), (<*), pure)
import Control.Category ((.))
import Control.Concurrent (threadDelay)
import Control.Concurrent.AdvSTM (AdvSTM, atomically, onCommit)
import Control.Concurrent.AdvSTM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.AdvSTM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Concurrent.Async (waitAny, withAsync)
import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import Control.Exception (Exception, IOException, throwIO, try)
import Control.Lens ((^.), view)
import Control.Monad (forever)
import Control.Monad.Morph (hoist, generalize)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT (EitherT))
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT, mapExceptT, throwE)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Lazy (keys)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Lazy as TL
import Data.Word (Word16)
import Database.Zookeeper (ZKError)
import Network.HTTP.Types (ok200, methodGet, methodPost, methodDelete, methodPut)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Servant.API ((:<|>) ((:<|>)), Raw)
import Servant.Server ( Server, ServerT, ServantErr (..), (:~>) (..)
                      , enter, serve, err404, err409 )
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)

data ApiConfig = ApiConfig { jobsDir :: FilePath
                           , zookeeperHosts :: ZKURI
                           , bindAddress :: String
                           , bindPort :: Word16
                           , subexecutorUser :: TL.Text }

$(deriveJSON defaultOptions ''ApiConfig)

bool :: a -> a -> Bool -> a
bool a b p = if p then b else a

fromExceptT :: ExceptT e m :~> EitherT e m
fromExceptT = Nat $ \x -> EitherT $ runExceptT x

throwExc :: (Exception e) => ExceptT e IO a -> IO a
throwExc act = runExceptT act >>= \case
    Left e  -> throwIO e
    Right a -> pure a

type Scheduler = ExceptT JobError EState ()

runPersist :: PersistContext -> Maybe (Persist.PersistAction ()) -> AdvSTM ()
runPersist _   Nothing  = return ()
runPersist ctx (Just p) = onCommit $ Persist.atomically ctx p

runScheduler :: AppConfig -> TVar AppState -> Scheduler -> ExceptT JobError IO ()
runScheduler conf stateV f = mapExceptT atomically $ do
    state <- lift $ readTVar stateV
    case runEState state $ runExceptT f of
        (Left  e, _ ) -> throwE e
        (Right _, ts) -> do
            lift . mapM_ (writeTChan $ schedChan conf) $ ts ^. scheduleCommands
            lift . writeTVar stateV $ ts ^. appState
            lift . runPersist (pctx conf) $ ts ^. persist

mainServer :: AppConfig -> TVar AppState -> Server VAPI
mainServer conf stateV = enter (fromExceptT . Nat (withExceptT onError)) server where
    server :: ServerT VAPI (ExceptT JobError IO)
    server = getJobsH :<|> getJobH :<|> getJobStateH :<|> killJobH :<|> deleteJobH :<|> createJobH

    getJobsH = lift $ getJobs <$> atomically (readTVar stateV)
    getJobH jid = (hoist generalize . getJob jid) =<< lift (atomically $ readTVar stateV)
    getJobStateH = fmap (view jobState) . getJobH
    createJobH = runScheduler' . createJob
    deleteJobH = runScheduler' . deleteJob

    killJobH jid (Failed UserKilled) = runScheduler' $ killJob jid
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

whileLeader :: ManagedZK -> BSS.ByteString -> IO a -> ExceptT LeadershipError IO a
whileLeader zk advertisedHost act =
    lift (runExceptT $ whenLeader zk "/eclogues" advertisedHost act) >>= \case
        Left LeadershipLost -> whileLeader zk advertisedHost act
        Left  e             -> throwE e
        Right a             -> pure a

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy { corsOrigins = Nothing
                                , corsMethods = [methodGet, methodPost, methodDelete, methodPut]
                                , corsRequestHeaders = ["content-type"]
                                , corsExposedHeaders = Nothing
                                , corsMaxAge = Nothing
                                , corsVaryOrigin = False
                                , corsRequireOrigin = False
                                , corsIgnoreFailures = False }

advertisedData :: ApiConfig -> BSS.ByteString
advertisedData (ApiConfig _ _ host port _) = BSL.toStrict $ encode (host, port)

withZK :: ApiConfig -> PersistContext -> Lock -> ManagedZK -> ExceptT LeadershipError IO ZKError
withZK apiConf pctx' webLock zk = whileLeader zk (advertisedData apiConf) $ do
    let jdir = jobsDir apiConf
    (followAuroraFailure, getURI) <- followAuroraMaster zk "/aurora/scheduler"
    stateV <- newTVarIO newAppState
    schedV <- newTChanIO
    createDirectoryIfMissing False jdir

    let conf = AppConfig jdir getURI schedV pctx'

    let web = Lock.with webLock $ run 8000 . myCors . serve (Proxy :: (Proxy VAPIWithDocs)) . docsServer $ mainServer conf stateV
        myCors = cors . const $ Just corsPolicy
        updater = forever $ goUpdate >> threadDelay (floor $ second (1 :: Double) `asVal` micro second)
        goUpdate = do
            uri <- atomically $ requireAurora conf
            (newStatusesRes, aJobs) <- do
                state <- atomically $ readTVar stateV
                let aJobs = activeJobs state
                    sconf = ScheduleConf jdir uri $ subexecutorUser apiConf
                newStatusesRes <- try . runExceptT . getSchedulerStatuses sconf $ keys aJobs
                pure (newStatusesRes, aJobs)
            case newStatusesRes of
                Right (Right newStatuses) -> atomically $ do
                    state <- readTVar stateV
                    let (_, ts) = runEState state $ updateJobs aJobs newStatuses
                    mapM_ (writeTChan schedV) $ ts ^. scheduleCommands
                    writeTVar stateV $ ts ^. appState
                    runPersist pctx' $ ts ^. persist
                Left (ex :: IOException)  ->
                    hPutStrLn stderr $ "Error connecting to Aurora at " ++ show uri ++ "; retrying: " ++ show ex
                Right (Left resp)         -> throwIO resp
        enacter = forever . atomically $ do -- TODO: catch run error and reschedule
            cmd <- readTChan schedV
            auri <- requireAurora conf
            let sconf = ScheduleConf jdir auri $ subexecutorUser apiConf
            onCommit . throwExc $ do
                runScheduleCommand sconf cmd
                lift . Persist.atomically pctx' $ Persist.deleteIntent cmd

    hPutStrLn stderr "Starting server on port 8000"
    withAsync web $ \webA -> withAsync updater $ \updaterA -> withAsync enacter $ \enacterA ->
        snd <$> waitAny [followAuroraFailure, const undefined <$> webA, updaterA, enacterA]

main :: IO ()
main = do
    apiConf <- orError =<< readJSON "/etc/xdg/eclogues/api.json"
    webLock <- Lock.new
    res <- Persist.withPersistDir (jobsDir apiConf) $ \pctx' ->
        lift . withZookeeper (zookeeperHosts apiConf) $
        runExceptT . withZK apiConf pctx' webLock
    case res of
        Left (LZKError e)         -> error $ "Zookeeper coordination error: " ++ show e
        Left (ActionException ex) -> throwIO ex
        Left LeadershipLost       -> error "impossibru!"
        Right e                   -> error $ "Aurora ZK lookup error: " ++ show e
