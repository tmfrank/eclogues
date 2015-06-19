{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude hiding ((.))

import Database.Zookeeper.Election (LeadershipError (..), whenLeader)
import Database.Zookeeper.ManagedEvents (ZKURI, ManagedZK, withZookeeper)
import Eclogues.API (VAPI, JobError (..), Health (Health))
import Eclogues.ApiDocs (VAPIWithDocs, apiDocsHtml)
import Eclogues.AppConfig (AppConfig (AppConfig, schedChan, pctx, auroraURI, schedJobURI, outputURI), requireAurora)
import Eclogues.Instances ()
import Eclogues.Persist (PersistContext)
import qualified Eclogues.Persist as Persist
import Eclogues.Scheduling.AuroraZookeeper (followAuroraMaster)
import Eclogues.Scheduling.Command ( ScheduleConf (ScheduleConf)
                                   , runScheduleCommand, getSchedulerStatuses, schedulerJobUI )
import Eclogues.State (getJobs, activeJobs, createJob, killJob, deleteJob, getJob, updateJobs)
import Eclogues.State.Monad (EState)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types (AppState)
import Eclogues.TaskSpec (Name, JobStatus, JobState (..), FailureReason (..), jobState, jobUuid)
import Eclogues.Util (readJSON, orError)
import Units

import Control.Applicative ((<$>), (<*), (*>), pure)
import Control.Category ((.))
import Control.Concurrent (threadDelay)
import Control.Concurrent.AdvSTM (AdvSTM, atomically, onCommit)
import Control.Concurrent.AdvSTM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.AdvSTM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Concurrent.Async (Async, waitAny, withAsync)
import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import Control.Exception (Exception, IOException, throwIO, try)
import Control.Lens ((^.), view)
import Control.Monad ((<=<), forever)
import Control.Monad.Morph (hoist, generalize)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT (EitherT))
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT, mapExceptT, throwE)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import qualified Data.ByteString.Lazy as BSL
import Data.Default.Generics (def)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (isJust)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word16)
import Database.Zookeeper (ZKError)
import Network.HTTP.Types (methodGet, methodPost, methodDelete, methodPut)
import Network.URI (URI)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Server ( Server, ServerT, ServantErr (..), (:~>) (..)
                      , enter, serve, err404, err409, err503, err303 )
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import System.Random (randomIO, mkStdGen, setStdGen)

data ApiConfig = ApiConfig { jobsDir :: FilePath
                           , zookeeperHosts :: ZKURI
                           , bindAddress :: String
                           , bindPort :: Word16
                           , subexecutorUser :: TL.Text
                           , outputUrlPrefix :: String }

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
    case ES.runEState state $ runExceptT f of
        (Left  e, _ ) -> throwE e
        (Right _, ts) -> do
            lift . mapM_ (writeTChan $ schedChan conf) $ ts ^. ES.scheduleCommands
            lift . writeTVar stateV $ ts ^. ES.appState
            lift . runPersist (pctx conf) $ ts ^. ES.persist

mainServer :: AppConfig -> TVar AppState -> Server VAPI
mainServer conf stateV = enter (fromExceptT . Nat (withExceptT onError)) server where
    server :: ServerT VAPI (ExceptT JobError IO)
    server = getJobsH :<|> getJobH :<|> getJobStateH :<|> killJobH :<|>
             mesosJob :<|> outputH :<|> deleteJobH :<|> createJobH :<|> healthH

    healthH = lift $ Health . isJust <$> atomically (auroraURI conf)
    getJobsH = lift $ getJobs <$> atomically (readTVar stateV)
    getJobH jid = (hoist generalize . getJob jid) =<< lift (atomically $ readTVar stateV)
    getJobStateH = fmap (view jobState) . getJobH
    createJobH spec = lift randomIO >>= runScheduler' . flip createJob spec
    deleteJobH = runScheduler' . deleteJob
    mesosJob = toMesos <=< getJobH where
        toMesos :: JobStatus -> ExceptT JobError IO ()
        toMesos js = (lift . atomically $ auroraURI conf) >>= \case
            Nothing  -> throwE SchedulerInaccessible
            Just uri -> throwE . SchedulerRedirect . schedJobURI conf uri $ js ^. jobUuid
    outputH name pathM = getJobH name *> throwE (SchedulerRedirect . outputURI conf name $ maybe "stdout" id pathM)

    killJobH jid (Failed UserKilled) = runScheduler' $ killJob jid
    killJobH jid _                   = throwE (InvalidStateTransition "Can only set state to Failed UserKilled") <* getJobH jid

    onError :: JobError -> ServantErr
    onError e = case e of
        NoSuchJob             -> err404 { errBody = encode NoSuchJob }
        SchedulerInaccessible -> err503 { errBody = encode SchedulerInaccessible }
        SchedulerRedirect uri -> err303 { errHeaders = [("Location", BSSC.pack uri)] }
        other                 -> err409 { errBody = encode other }
    runScheduler' = runScheduler conf stateV

docsServer :: Server VAPI -> Server VAPIWithDocs
docsServer = (:<|> serveDocs) where
    serveDocs = lift $ pure apiDocsHtml

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
advertisedData (ApiConfig _ _ host port _ _) = BSL.toStrict $ encode (host, port)

mkOutputURI :: ApiConfig -> Name -> FilePath -> String
mkOutputURI conf name path = (outputUrlPrefix conf) ++ TL.unpack name ++ "/" ++ path

withZK :: ApiConfig -> Lock -> ManagedZK -> ExceptT LeadershipError IO ZKError
withZK apiConf webLock zk = whileLeader zk (advertisedData apiConf) $ do
    let jdir = jobsDir apiConf
    schedV <- newTChanIO
    (followAuroraFailure, getURI) <- followAuroraMaster zk "/aurora/scheduler"
    createDirectoryIfMissing False jdir
    Persist.withPersistDir jdir $ \pctx' -> do
        let conf = AppConfig jdir getURI schedV pctx' (schedulerJobUI . TL.unpack $ subexecutorUser apiConf) (mkOutputURI apiConf)
            mkConf = ScheduleConf jdir $ subexecutorUser apiConf
        lift $ withPersist mkConf conf webLock followAuroraFailure

withPersist :: (URI -> ScheduleConf) -> AppConfig -> Lock -> Async ZKError -> IO ZKError
withPersist mkConf conf webLock followAuroraFailure = do
    (_, st) <- ES.runEStateT def $ do
        (js, cmds) <- lift . Persist.atomically (pctx conf) $ do
            js <- Persist.allJobs
            cmds <- Persist.allIntents
            pure (js, cmds)
        ES.loadJobs js
        lift . atomically $ mapM_ (writeTChan $ schedChan conf) cmds
    stateV <- newTVarIO $ st ^. ES.appState
    let web = Lock.with webLock $ run 8000 . myCors . serve (Proxy :: (Proxy VAPIWithDocs)) . docsServer $ mainServer conf stateV
        myCors = cors . const $ Just corsPolicy
        updater = forever $ goUpdate >> threadDelay (floor $ second (1 :: Double) `asVal` micro second)
        goUpdate = do
            auri <- atomically $ requireAurora conf
            (newStatusesRes, aJobs) <- do
                state <- atomically $ readTVar stateV
                let aJobs = activeJobs state
                newStatusesRes <- try . runExceptT . getSchedulerStatuses (mkConf auri) $ HashMap.elems aJobs
                pure (newStatusesRes, aJobs)
            case newStatusesRes of
                Right (Right newStatuses) -> atomically $ do
                    state <- readTVar stateV
                    let (_, ts) = ES.runEState state $ updateJobs aJobs newStatuses
                    mapM_ (writeTChan $ schedChan conf) $ ts ^. ES.scheduleCommands
                    writeTVar stateV $ ts ^. ES.appState
                    runPersist (pctx conf) $ ts ^. ES.persist
                Left (ex :: IOException)  ->
                    hPutStrLn stderr $ "Error connecting to Aurora at " ++ show auri ++ "; retrying: " ++ show ex
                Right (Left resp)         -> throwIO resp
        enacter = forever . atomically $ do -- TODO: catch run error and reschedule
            cmd <- readTChan $ schedChan conf
            auri <- requireAurora conf
            onCommit . throwExc $ do
                runScheduleCommand (mkConf auri) cmd
                lift . Persist.atomically (pctx conf) $ Persist.deleteIntent cmd

    hPutStrLn stderr "Starting server on port 8000"
    withAsync web $ \webA -> withAsync updater $ \updaterA -> withAsync enacter $ \enacterA ->
        snd <$> waitAny [followAuroraFailure, const undefined <$> webA, updaterA, enacterA]

seedStdGen :: IO ()
seedStdGen = do
    curTime <- getPOSIXTime
    -- Unlikely to start twice within a millisecond
    let pico = floor (curTime * 1e3) :: Int
    setStdGen $ mkStdGen pico

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
